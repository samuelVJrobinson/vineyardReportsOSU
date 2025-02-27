#' @title Make vineyard reports
#' @description Create vineyard reports from iNaturalist project data, bee/plant interactions, and plant list.
#' 
#' @param plantListPath List of plants from Oregon Flora (csv)
#' @param beeDataPath List of bee/plant interactions from OBA
#' @param csvPaths Folder containing vineyard iNaturalist csv files 
#' @param vinePlDatAll Output csv of all vineyard records. Skips writing if NULL
#' @param predictedBeesPath Output csv of Predicted bees for each vineyard. Skips writing if NULL
#' @param dataStoragePath Storage path for .Rdata file
#' 
#' @return Nothing - writes to dataStoragePath
#' @export
#'
#' @examples
organizeData <- function(plantListPath, 
                         beeDataPath, 
                         iNatFolder = NULL, 
                         vinePlDatAll = NULL,
                         predictedBeesPath = NULL,
                         dataStoragePath = NULL
){
  
  # Preamble ---------------------------
  
  ##Input arguments
  # plantListPath : './data/plant data/cleanedPlantList2024.csv' #List of plants from Oregon Flora
  # beeDataPath : './data/bee data/OBA_2017_2023_v16Oct24.csv' #Bee list from OBA
  # iNatFolder =  Get paths to vineyard iNaturalist csvs
  ## Writing (output) arguments
  # vinePlDatAll = Output csv of all vineyard records
  # predictedBeesPath = Output csv of Predicted bees for each vineyard
  # dataStoragePath = Storage path for .Rdata file
  
  ## Included in /data:
  # famGenPath:  #Bee genus-family lookup table - included in /data
  # orCountyShpPath : Oregon county polygons - included in /data
  # orEcoregShpPath : Ecoregion county polygons - included in /data 
  # beeAbstractsPath = Bee/plant abstracts - included in /data
  
  library(tidyverse)
  library(sf)
  library(vegan)
  library(rmarkdown)
  library(knitr)
  # library(bipartite)
  
  #Debugging
  # setwd("C:/Users/Samuel/Documents/Projects/Stats projects/OSU-vineyard-reports")
  
  #List of plants from Oregon Flora
  plantListPath = 'C:\\Users\\s_robinson\\OneDrive - Ducks Unlimited Canada\\Desktop\\Test vineyard folders\\cleanedPlantList2024.csv' 
  beeDataPath = 'C:\\Users\\s_robinson\\OneDrive - Ducks Unlimited Canada\\Desktop\\Test vineyard folders\\OBA_2017_2023_v16Oct24.csv' 
  iNatFolder =  'C:\\Users\\s_robinson\\OneDrive - Ducks Unlimited Canada\\Desktop\\Test vineyard folders\\iNat records'
  vinePlDatAll = NULL; predictedBeesPath = NULL
  # source('./R/capFirst.R'); 
  source('./R/getPlGenScores.R'); source('./R/makeGenSpp.R')
  source('./R/replaceSynonyms.R'); source('./R/secConn.R'); source('./R/st_within_fast.R')
  
  #Built-in paths
  famGenPath <- './data/famGenLookup.csv' #Bee genus-family lookup table
  orCountyShpPath <- "./data/shapefiles/orcntypoly.shp" #Oregon county polygons
  orEcoregShpPath <- "./data/shapefiles/or_eco_l3.shp" #Ecoregion county polygons
  beeAbstractsPath <- './data/Bee_Abstracts.csv' #Bee abstracts
  
  #Get paths to vineyard iNaturalist csvs
  csvPaths <- list.files(iNatFolder,full.names = TRUE, recursive = TRUE,pattern = '.csv') #Gets list of csvs in "./data" folder
  
  # #Vineyard "highlights"
  # beePlantPairsPath <- './data/highlightBeesPlants_paired.csv' #Unique bee/plant pairs for each vineyard: bees/plants must be linked
  # beePlantPairsPath2 <- './data/highlightBeesPlants_unpaired.csv' #Unique bee/plant pairs for each vineyard: bees/plants don't have to be linked
  
  famGen <- read.csv(famGenPath) %>% rename(lookupFam=family) #Bee genus-family lookup table
  
  # Load and clean up Oregon plant data -------------------------
  plantList <- read.csv(plantListPath,strip.white = TRUE) 
  
  reqPlantCols <- c('Scientific_name','Synonym','Common_name','Bloom_start',
                    'Bloom_end','Lifecycle','Origin','Garden_type','Family','PlantAbstract')
  
  if(any(!reqPlantCols %in% colnames(plantList))){
    stop(paste0('Plant list from Oregon Flora must have the following columns:\n',paste0(reqPlantCols,collapse='\n')))
  }
  
  chooseThese <- grepl('(var|ssp)\\.',plantList$Scientific_name)
  if(any(chooseThese)){
    warning(paste0("Plant names with 'var' or 'ssp' found in plant list. Excluded ",sum(chooseThese)," from plant list\n",
                   paste(plantList$Scientific_name[chooseThese],collapse = '\n')))
    plantList <- plantList %>% filter(!chooseThese)  
  }
  
  chooseThese <- grepl('^\\S+(ales|eae|dae|nae)$',plantList$Scientific_name)
  if(any(chooseThese)){
    warning(paste0("Orders, families, or other non-genus groups found in plant list. Excluded ",sum(chooseThese)," from plant list\n",
                   paste(plantList$Scientific_name[chooseThese],collapse = '\n')))
    plantList <- plantList %>% filter(!chooseThese)  
  }
  
  chooseThese <- grepl('\\s.*\\s.*$',plantList$Scientific_name)
  if(any(chooseThese)){
    warning(paste0("Triple names (possibly varieties or culivars) found in plant list. Excluded ",sum(chooseThese)," from plant list\n",
                   paste(plantList$Scientific_name[chooseThese],collapse = '\n')))
    plantList <- plantList %>% filter(!chooseThese)  
    # # Could fix by removing the end of triple-names (varieties) - leads to problems with duplicate plant names - let the user figure this out
    # mutate(Scientific_name=sapply(strsplit(Scientific_name,' '),function(x) paste0(x[1:pmin(length(x),2)],collapse=' '))) 
  }
  
  plantList <- plantList %>% 
    filter(Scientific_name!='') %>% rowwise() %>% #Removes blanks
    mutate(Scientific_name=ifelse(grepl('\\s',Scientific_name),Scientific_name,paste0(Scientific_name,' spp.'))) %>% #Adds spp to genus
    mutate(Synonym=ifelse(grepl('.spp',Scientific_name)&Synonym!='',paste0(Synonym,' spp.'),Synonym)) %>% #Adds spp to Synonym 
    mutate(Common_name=str_to_title(gsub(',.*','',Common_name))) %>% #Removes all but first common name, and capitalizes
    mutate(Lifecycle=str_to_title(Lifecycle)) %>% 
    rename(isNoxious=Noxious_weed,isWeedy=Weedy_species) 
  
  #Adds genera to list if not already present
  plGen <- unique(gsub('\\s.*','',plantList$Scientific_name)) #Unique plant genera
  noGen <- plGen[!plGen %in% gsub('\\s.*','',plantList$Scientific_name[grepl('.spp',plantList$Scientific_name)])] #Listed plant species with no generic-level (X spp.) record
  if(length(noGen)>0){
    warning(paste0('Genus-level information for ',length(noGen),' listed plant species missing. Adding missing genera:',
                   paste(c('\n',noGen),collapse = '\n')))
    cstring <- function(x) paste0(unique(unlist(strsplit(x,', '))),collapse=', ')
    
    #Gets plants that don't have a genus-level record, and amalgamates lifecycle, origin, and garden info
    plantList <- plantList %>% select(Scientific_name,Lifecycle,Origin:Garden_type,Family) %>% 
      mutate(Scientific_name=gsub('\\s.*','',Scientific_name)) %>% 
      filter(Scientific_name %in% noGen) %>% mutate(Scientific_name=paste0(Scientific_name,' spp.')) %>% 
      group_by(Scientific_name) %>% summarize(across(c(Lifecycle,Origin,Garden_type),cstring),
                                              across(c(isNoxious,isWeedy),~any(!.x))) %>% 
      bind_rows(plantList) #Adds to the original plant list  
  }
   
  #Adds columns of plant traits 
  plantList <- plantList %>% 
    mutate(isNative=grepl('(N|n)ative',Origin,)) %>%
    mutate(isWeedy=ifelse(isNoxious,FALSE,isWeedy)) %>% #Removes noxious species from weedy (non-overlapping sets)
    mutate(isLandscape=grepl('landscape',Garden_type),isEdge=grepl('edge areas',Garden_type),
           isRiparian=grepl('riparian',Garden_type),isOpen=grepl('open areas',Garden_type),
           isOakWoodland=grepl('oak woodland',Garden_type),isWetland=grepl('seasonally wet',Garden_type)) #Assigns garden type - clunky, but works
  
  #Test for duplicate names
  if(any(table(plantList$Scientific_name)>1)){
    stop(paste0('Duplicate scientific names found in plant list: ',
                paste0(names(which(table(plantList$Scientific_name)>1)),collapse = ', ')))
  }
  
  #List of non-native plants - also includes ones not found in complete plant list (possibly misidentified)
  nonNativeGen <- plantList %>% select(Scientific_name,isNative) %>%
    mutate(Scientific_name=gsub('\\s.*$','',Scientific_name)) %>% group_by(Scientific_name) %>%
    summarize(anyNative=any(isNative)) %>% filter(!anyNative) %>% pull(Scientific_name)
  
  nonNativeSpp <- plantList %>% select(Scientific_name,isNative) %>% 
    filter(grepl('\\s',Scientific_name)) %>% filter(!isNative) %>% 
    pull(Scientific_name)
  
  # Load and clean up bee data ------------------------------------
  beeData <- read.csv(beeDataPath,stringsAsFactors = FALSE,
                      strip.white = TRUE,na.strings=c('NA',''))
  
  reqBeeCols <- c('Collectors.1','sex','Associated.plant','Collectionmethod',
                  'MonthJul','MonthAb','Year.1','County','Genus','Species',
                  'Dec..Lat.','Dec..Long.')
  
  if(any(!reqBeeCols %in% colnames(beeData))){
    stop(paste0('Bee data must have the following columns:\n',paste0(reqBeeCols,collapse='\n')))
  }
  
  beeData <- beeData %>% filter(!is.na(Genus)) %>%
    transmute(recordedBy=Collectors.1,sex=sex,foragePlant=Associated.plant,
              samplingProtocol=Collectionmethod,date=paste(MonthJul,MonthAb,Year.1),county=County,
              order='Hymenoptera',family=NA,genus=Genus,species=Species,genSpp=NA,plantGenus=NA,plantSpp=NA,
              lat=Dec..Lat.,lon=Dec..Long.) %>%
    mutate(across(where(is.character),~str_trim(.))) %>% #Trim whitespace across columns
    mutate(date=as.Date(date,format='%B %d %Y')) %>% #Create date
    mutate(genus=str_to_title(genus),family=famGen$lookupFam[match(genus,famGen$genus)]) %>%  #Capitalize spp names
    mutate(family=ifelse(genus=='Anthophorini','Apidae',family),family=ifelse(genus=='Anthophorini',NA,family)) %>% #Fix tribe name
    makeGenSpp(genus,species) #%>% #Make genSpp column
  
  chooseThese <- grepl('\\s\\(.+$',beeData$foragePlant) #Gets rid of brackets+text after foragePlant
  if(any(chooseThese)){
    warning(paste0("Removed brackets and extra text after intial foragePlant name in bee list. Altered ",sum(chooseThese)," records from bee list\n",
                   paste(apply(cbind(unique(na.omit(beeData$foragePlant[chooseThese])),
                                     paste(gsub('\\s\\(.+$','',unique(na.omit(beeData$foragePlant[chooseThese]))))),1,
                               paste,collapse=' -> '),collapse='\n')))
    beeData <- beeData %>% mutate(foragePlant=gsub('\\s\\(.+$','',foragePlant)) 
  }
  
  chooseThese <- grepl('(\\s.\\s.*$|\\s.$)',beeData$foragePlant)#Gets rid of hybrid x marks
  if(any(chooseThese)){
    warning(paste0("Hybrid names, x marks, or other non-standard text found in foragePlant names in bee list. Altered ",sum(chooseThese)," records from bee list\n",
                   paste(apply(cbind(unique(na.omit(beeData$foragePlant[chooseThese])),
                                     paste(gsub('(\\s.\\s.*$|\\s.$)','',unique(na.omit(beeData$foragePlant[chooseThese]))))),1,
                               paste,collapse=' -> '),collapse='\n')))
    beeData <- beeData %>% mutate(foragePlant=gsub('(\\s.\\s.*$|\\s.$)','',foragePlant)) 
  }
  
  
  chooseThese <- grepl('(^\\S+(ales|eae|dae|nae)$|Composite)',beeData$foragePlant) #Gets rid of higher-level names
  if(any(chooseThese)){
    warning(paste0("Orders, families, or other non-genus groups found in foragePlant names in bee list. Removed ",sum(chooseThese)," records from bee list\n",
                   paste(unique(na.omit(beeData$foragePlant[chooseThese])),collapse='\n')))
    beeData <- beeData %>% mutate(foragePlant=ifelse(grepl('(^\\S+(ales|eae|dae|nae)$|Composite)',foragePlant),NA,foragePlant)) 
  }
  
  chooseThese <- grepl('(,|^\\S+\\s\\S+\\s.*$)',beeData$foragePlant) #Gets rid of lists of foragePlant species
  if(any(chooseThese)){
    warning(paste0("Lists of plants or triple-name varietals found in foragePlant names in bee list. Removed ",sum(chooseThese)," records from bee list\n",
                   paste(unique(na.omit(beeData$foragePlant[chooseThese])),collapse='\n')))
    beeData <- beeData %>% mutate(foragePlant=ifelse(grepl('(,|^\\S+\\s\\S+\\s.*$)',foragePlant),NA,foragePlant)) 
  }
  
  beeData <- beeData %>% 
    mutate(foragePlant=case_when( #Get rid of weird plant records
      grepl('^(N|n)et\\s*$',foragePlant) ~ NA, #"Net"
      !grepl('\\s',foragePlant) & !is.na(foragePlant) ~ paste0(foragePlant,' spp.'), #Adds "spp." to singletons
      .default = gsub('sp+\\.*$','spp.',as.character(foragePlant)
      ))) %>% ungroup() %>% 
    mutate(foragePlant=replaceSynonyms(foragePlant,plantList$Scientific_name,plantList$Synonym)) %>% #Replace plant synonyms
    mutate(plantGenus=gsub("\\s.+$","",foragePlant), #Separates foragePlant into genus and spp
           plantSpp=ifelse(grepl(" ",foragePlant),gsub("^\\w+\\s","",foragePlant),NA)) %>%
    mutate(plantSpp=ifelse(plantSpp=='spp.',NA,plantSpp)) %>%
    filter(!is.na(lat)&!is.na(lon)) %>%
    st_as_sf(coords=c('lon','lat')) %>% #Set lon and lat as coordinates
    st_set_crs(4269) %>% #Set coordinate reference system (NAD83)
    st_transform(3643) #Transform to Oregon Lambert system
  
  # Load spatial data ------------------------
  
  #Shapefiles of Oregon counties
  orCounties <- st_read(orCountyShpPath,quiet = TRUE) #Read in county polygons
  
  if(any(!c('altName','geometry') %in% colnames(orCounties))){
    stop(paste0('Oregon county shapefiles must have the following columns:\n',paste0('altName',collapse='\n')))
  }
  
  orCounties <- orCounties %>% select(altName) %>% #st_set_crs(4269) %>% #NAD83
    st_transform(3643)
  
  #Shapefiles of Oregon ecoregions
  
  orEcoReg <- st_read(orEcoregShpPath,quiet = TRUE)
  if(any(!c('NA_L3NAME','geometry') %in% colnames(orEcoReg))){
    stop(paste0('Oregon ecoregion shapefiles must have the following columns:\n',paste0('NA_L3NAME',collapse='\n')))
  }
  
  orEcoReg <- orEcoReg %>% transmute(name=NA_L3NAME) %>% #Read in ecoregions
    st_transform(3643) %>% #Transform to Oregon Lambert
    mutate(name=gsub(' and ',' & ',name)) %>% #Replace "and" with "&"
    mutate(name=gsub('Cascades Slopes','Cascades\nSlopes',name)) #Add line break (\n) to "Cascades Slopes"
  
  #Overwrite county name using shapefiles, using lat/lon data from individual records
  
  #Get county index for each location
  beeData$county <- orCounties$altName[st_within_fast(beeData,orCounties)] #Overwrite county
  if(any(is.na(beeData$county))){
    warning(paste0(sum(is.na(beeData$county)),' bee samples not within Oregon counties discarded'))
    beeData <- beeData %>% filter(!is.na(county))
  }
  
  #Get Oregon ecoregion
  beeData$ecoreg <- gsub('\n',' ',orEcoReg$name)[st_within_fast(beeData,orEcoReg)] #Get rid of carriage return in ecoregion name
  if(any(is.na(beeData$ecoreg))){
    warning(paste0(sum(is.na(beeData$ecoreg)),' bee samples not within Oregon ecoregions discarded'))
    beeData <- beeData %>% filter(!is.na(ecoreg))
  }
  
  #Test for repeated spaces
  if(any(apply(st_drop_geometry(beeData),2,function(x) any(grepl('  ',sort(unique(x))))))){
    stop('Double spaces "  " found in bee data')
    apply(st_drop_geometry(beeData),2,function(x) any(grepl('  ',sort(unique(x)))))
  }
  
  mi2km <- 1.609344 #Miles per kilometer
  
  #Common names for bee families
  commonFam <- data.frame(family=factor(c('Andrenidae','Apidae','Colletidae','Halictidae','Megachilidae')),
                          common=c('Mining bees','Social bees','Polyester bees','Sweat bees','Leaf-cutting bees')) %>%
    mutate(plotLab=paste0(family,'\n(',common,')'))
  
  #Load and clean up iNaturalist records ------------------------
  
  #Function to get CSV files
  getCSVs <- function(x){ 
    l <- read.csv(x,strip.white = TRUE) #Read in csvs
    reqCols <- c('latitude','longitude','scientific_name','common_name','observed_on') #Required columns names
    if(any(!reqCols %in% colnames(l))){ #If observed_on not found in iNat record
      stop(paste0('Columns missing from iNaturalist record: ',x,'. Required columns: ',paste0(reqCols,collapse = ', ')))
    }
    l <- l %>% select(latitude,longitude,scientific_name,common_name,observed_on) %>% #Select relevant columns
      mutate(vineyard=gsub('(^.+\\d{4}/|\\.csv$)','',x)) %>% #Gets vineyard name and year
      mutate(year=format(as.Date(observed_on),format='%Y')) %>% select(-observed_on)
    return(l)
  }
  
  #Get all CSVs and assemble into single dataframe
  vinePlDat <- lapply(csvPaths,getCSVs) %>% 
    bind_rows() %>% 
    filter(!grepl('eae$',scientific_name)) %>% #Removes family
    filter(!scientific_name %in% unique(beeData$genSpp)) %>% #Removes bee names (didn't record plant)
    mutate(scientific_name=gsub('(\\s.\\s.*$|\\s.$)','',scientific_name)) %>% #Removes hybrid "x" markings
    mutate(scientific_name=ifelse(!grepl('\\s',scientific_name),paste0(scientific_name,' spp.'),scientific_name)) %>% #Adds spp. to end of genus
    mutate(scientific_name=sapply(strsplit(scientific_name,'\\s'),function(x) paste0(x[1:2],collapse=' '))) %>% #Drops last word in triple names (e.g. Eriophyllum lanatum integrifolium -> Eriophyllum lanatum)
    mutate(scientific_name=replaceSynonyms(scientific_name,plantList$Scientific_name,plantList$Synonym)) %>% #Replace plant synonyms
    mutate(plGenus=gsub('\\s.*','',scientific_name)) %>% 
    mutate(common_name=str_to_title(gsub(',.*','',common_name))) %>% #Capitalizes common names, and chooses only first one (if separated by commas)
    st_as_sf(coords=c('longitude','latitude')) %>% #Set lon and lat as coordinates
    st_set_crs(4269) %>% #Set coordinate reference system (NAD83)
    st_transform(3643)
  
  #Join ecoregions and counties to vineyard data
  vinePlDat$ecoreg <- gsub('\n',' ',orEcoReg$name)[st_within_fast(vinePlDat,orEcoReg)] #Get rid of carriage return in ecoregion name
  if(any(is.na(vinePlDat$ecoreg))){
    warning(paste0(sum(is.na(vinePlDat$ecoreg)),' samples not matching Oregon ecoregions discarded'))
    vinePlDat <- vinePlDat %>% filter(!is.na(ecoreg))
  }
  if(!is.null(vinePlDatAll)){
    #Write to single csv
    vinePlDat %>% st_transform(4269) %>% 
      mutate(lat=st_coordinates(.)[2],lon=st_coordinates(.)[1]) %>% 
      st_drop_geometry() %>% 
      write.csv(file = vinePlDatAll,row.names = FALSE)
  }
  
  # Make regional and vineyard-level networks ------------------------------------
  
  # Gets unique plant records and associated bee records from the 2024 bee data. Used to generate a list of "highlight" bees and plants for growers
  
  #Figure out how Rosmarinus is getting "super" classification
  #Phacelia tanacetifolia should be in Willamette Valley network
  
  #Create ecoregions-specific networks (all interactions from a given region). Should eventually turn into a stand-alone function. Could also make a general-purpose version that works with arbitrary subsets (would replace both getRegNtwks and getVyNtwks?)
  getRegNtwks <- function(nm,bdat,pList){ 
    if(nm=='ALL'){ #Uses all of Oregon
      d <- bdat %>% st_drop_geometry()
    } else {
      d <- bdat %>% filter(ecoreg==nm) %>% st_drop_geometry() #Filters data to ecoregion. Could use some kind of tidyr nested dataframe approach
    }
    
    #dataframe: Bee spp -> flower spp
    sppList <- d %>% filter(!is.na(species),!is.na(plantSpp)) %>%
      select(genSpp,foragePlant) %>% na.omit() %>%
      group_by(foragePlant) %>%
      summarize(beeSpp=paste0(genSpp,collapse = ','))
    
    #dataframe: Bee spp -> plant genera (plant spp unknown)
    genList <- d %>% filter(!is.na(species)) %>%
      select(genSpp,plantGenus) %>% na.omit() %>%
      group_by(plantGenus) %>%
      summarize(beeSpp=paste0(unique(genSpp),collapse = ',')) %>%
      rename(foragePlant=plantGenus)
    
    #matrix: Bee spp -> plant spp (all)
    ntwk_all <- d %>% filter(!is.na(species),!is.na(plantSpp)) %>%
      select(genSpp,foragePlant) %>% na.omit() %>%
      left_join(select(pList,Scientific_name,isWeedy,isNoxious,isNative),
                by = c('foragePlant'='Scientific_name')) %>%
      count(genSpp,foragePlant) %>%
      pivot_wider(names_from=foragePlant,values_from=n,values_fill = 0) %>%
      column_to_rownames('genSpp') %>% as.matrix() %>% t()
    
    #matrix: Bee spp -> plant spp (nonweedy natives only)  - recommended plant spp for growers
    ntwk_noWeed <- d %>% filter(!is.na(species),!is.na(plantSpp)) %>%
      select(genSpp,foragePlant) %>% na.omit() %>%
      left_join(select(pList,Scientific_name,isWeedy,isNoxious,isNative),
                by = c('foragePlant'='Scientific_name')) %>%
      filter(!is.na(isWeedy),!isNoxious,!isWeedy,isNative) %>%
      count(genSpp,foragePlant) %>%
      pivot_wider(names_from=foragePlant,values_from=n,values_fill = 0) %>%
      column_to_rownames('genSpp') %>% as.matrix() %>% t()
    
    #matrix: Bee genus -> plant genus (all)
    ntwk_gen_all <- d %>% select(genus,plantGenus) %>% na.omit() %>% 
      count(genus,plantGenus) %>%
      pivot_wider(names_from=plantGenus,values_from=n,values_fill = 0) %>%
      column_to_rownames('genus') %>% as.matrix() %>% t()
    
    #matrix: Bee species -> plant genus (all) 
    ntwk_genSpp_all <- d %>% filter(!is.na(species)) %>%
      select(genSpp,plantGenus) %>% na.omit() %>%
      count(genSpp,plantGenus) %>%
      pivot_wider(names_from=plantGenus,values_from=n,values_fill = 0) %>%
      column_to_rownames('genSpp') %>% as.matrix() %>% t()
    
    #Temporary plant list with only genus-level info
    plantList2 <- pList %>% select(Scientific_name,isWeedy,isNoxious,isNative) %>% 
      mutate(Genus_name=gsub('\\s.*$','',Scientific_name)) %>% 
      group_by(Genus_name) %>% 
      #"Abraham's bargain": is there at least 1 nonweedy, nonnoxious, and native species?
      summarize(isNonWeedy=any(!isWeedy),isNonNoxious=any(!isNoxious),isNative=any(isNative))
    
    #matrix: Bee species -> plant genus (all) - Recommended plant species for growers
    ntwk_genSpp_noWeed <- d %>% filter(!is.na(species)) %>%
      select(genSpp,plantGenus) %>% na.omit() %>%
      left_join(plantList2,by = c('plantGenus'='Genus_name')) %>%
      filter(!is.na(isNonWeedy),isNonNoxious,isNonWeedy,isNative) %>%
      count(genSpp,plantGenus) %>%
      pivot_wider(names_from=plantGenus,values_from=n,values_fill = 0) %>%
      column_to_rownames('genSpp') %>% as.matrix() %>% t()
    
    #Which bees are rare? - using a simple definition (>median)
    rareBees <- d %>% filter(!is.na(species)) %>% 
      count(genSpp) %>% arrange(desc(n)) %>%
      filter(n<median(n))
    
    if(any(dim(ntwk_noWeed)==0)){
      if(any(dim(ntwk_all)==0)){
        warning('No plant species data found for ',nm)
      } else{
        warning('No non-weedy species plant data for ',nm)  
      }
      topSpp <- topGen <- NA
    } else {
      #Top plant species for ecoregion (based on Chao1 richness from plant spp - bee spp network)
      topSpp <- vegan::estimateR(ntwk_all) %>% t() %>% 
        data.frame() %>% 
        rownames_to_column('plantSpp') %>%
        select(plantSpp:S.chao1) %>% arrange(desc(S.chao1)) %>% 
        rename(Nbees=S.obs,Nbees_estim=S.chao1) %>%
        mutate(Nbees_rare=rowSums(ntwk_all[,colnames(ntwk_all) %in% rareBees$genSpp,drop=FALSE])) %>%
        arrange(desc(Nbees)) %>%
        #Creates plant "quality" rankings, based on number of rare bees hosted (>0) and overall visitor richness (>median)
        mutate(quality=case_when( #Cutoffs for forage plant "quality"
          Nbees_estim >median(Nbees_estim) & Nbees_rare >0 ~ 'super',
          xor(Nbees_estim > median(Nbees_estim),Nbees_rare >0) ~ 'good',
          TRUE ~ 'poor')) %>% 
        mutate(quality=factor(quality,levels=c('super','good','poor')))
    }
    
    if(any(dim(ntwk_genSpp_noWeed)==0)){
      if(any(dim(ntwk_genSpp_all)==0)){
        warning('No plant genus data found for ',nm)
      } else{
        warning('No non-weedy plant genus data for ',nm)  
      }
      topGen <- NA
    } else {
      #Top plant genera (Chao1 richness from plant genus - bee spp network)
      topGen <- vegan::estimateR(ntwk_genSpp_all) %>% t() %>% 
        data.frame() %>% 
        rownames_to_column('plantGen') %>%
        select(plantGen:S.chao1) %>% arrange(desc(S.chao1)) %>% 
        rename(Nbees=S.obs,Nbees_estim=S.chao1) %>%
        mutate(Nbees_rare=rowSums(ntwk_genSpp_all[,colnames(ntwk_genSpp_all) %in% rareBees$genSpp,drop=FALSE])) %>%
        arrange(desc(Nbees)) %>%
        #Creates plant "quality" rankings, based on number of rare bees hosted (>0) and overall visitor richness (>median)
        mutate(quality=case_when( #Cutoffs for forage plant "quality"
          Nbees_estim >median(Nbees_estim) & Nbees_rare >0 ~ 'super',
          xor(Nbees_estim > median(Nbees_estim),Nbees_rare >0) ~ 'good',
          TRUE ~ 'poor')) %>% 
        mutate(quality=factor(quality,levels=c('super','good','poor')))
    }
    
    #Output list
    return(list('sppList'=sppList,'genList'=genList,
                'ntwk_all'=ntwk_all,'ntwk_noWeed'=ntwk_noWeed,
                'ntwk_gen_all'=ntwk_gen_all,'ntwk_genSpp_all'=ntwk_genSpp_all,
                'topSpp'=topSpp,'topGen'=topGen,'rareBees'=rareBees))
  }
  
  #Assemble regional networks into a list
  ecoRegNetworks <- lapply(c(orEcoReg$name,'ALL'),getRegNtwks,bdat=beeData,pList=plantList) %>% 
    set_names(c(orEcoReg$name,'ALL'))
  
  #Get unique interaction matrices for each unique vineyard
  vyNames <- gsub('.csv','',sort(unique(basename(csvPaths)))) #Unique vineyard names from csv paths
  
  #Get networks for individual vineyards from regional data
  getVyNtwks <- function(vy,vpDat,erNtwk){ #Check variable scoping...
    vyPlantSpp <- vpDat %>% st_drop_geometry() %>% 
      filter(vineyard==vy) %>% #Plant species list for this vineyard
      distinct() %>% filter(scientific_name!=plGenus) %>% 
      pull(scientific_name)
    vyPlantGen <- unique(gsub('\\s.+$','',vyPlantSpp)) #Plant genus list for this vineyard
    vyEcoreg <- unique(vpDat$ecoreg[vpDat$vineyard==vy]) #Ecoregion for this vineyard
    if(length(vyEcoreg)!=1) stop('More than 1 ecoregion per vineyard')
    #Ecoregion network
    ecoregNtwk_summary <- c('ecoRegName'=vyEcoreg,
                            lapply(list('ntwk_all'=erNtwk[[vyEcoreg]]$ntwk_all,
                                        'ntwk_noWeed'=erNtwk[[vyEcoreg]]$ntwk_noWeed),function(x){
                              c('Nsamples'=sum(x),'Nrichness'=sum(x>0),
                                'Nplants'=nrow(x),'Nbees'=ncol(x))  
                            })) 
    #Lists of species and genus (subset of regional lists)
    m <- list(
      #List of plant species along with associated bee species
      'sppList'= filter(erNtwk[[vyEcoreg]]$sppList,foragePlant %in% vyPlantSpp),
      #List of plant genera along with associated bee species
      'genList'= filter(erNtwk[[vyEcoreg]]$genList,foragePlant %in% vyPlantGen) 
    )
    
    if(any(nrow(m)==0)) warning(paste('No plants from',vy,'vineyard found in',vyEcoreg,'plant list'))
    
    #Matrices of: full vineyard network + Non-weedy vineyard network
    n <- lapply(list('ntwk'=erNtwk[[vyEcoreg]]$ntwk_all, #Uses ecoregion network
                     'ntwk_noWeed'=erNtwk[[vyEcoreg]]$ntwk_noWeed, #Ecoregion network without weeds
                     'ntwk_all'=erNtwk[['ALL']]$ntwk_all, #Uses full oregon network
                     'ntwk_all_noWeed'=erNtwk[['ALL']]$ntwk_noWeed #Full oregon network without weeds
    ),function(x){
      if(any(rownames(x) %in% vyPlantSpp)){
        x <- x[rownames(x) %in% vyPlantSpp,,drop=FALSE] #Remove plants that aren't found in plant name list
        x <- x[,colSums(x)>0,drop=FALSE] #Remove bees that have no interactions
        return(x)
      } else {
        warning(paste('No plants from',vy,'vineyard found in',vyEcoreg,'interaction network'))
        return(NA)
      } 
    })
    return(c(list('ecoregNtwk_summary'=ecoregNtwk_summary),m,n))
  }
  
  vyNetworks <- lapply(vyNames,getVyNtwks,vpDat=vinePlDat,erNtwk=ecoRegNetworks) %>% 
    set_names(vyNames)
  
  #Write predicted bees at each vineyard to a csv
  if(!is.null(predictedBeesPath)){
    lapply(vyNetworks,function(x){
      if(!'matrix' %in% class(x$ntwk_noWeed)){
        x <- matrix(1,dimnames = list('NA','NA'))
      } else {
        x <- x$ntwk_noWeed
      }
      data.frame(x) %>% 
        rownames_to_column('PlantSpp') %>% 
        pivot_longer(-PlantSpp,names_to = 'BeeSpp',
                     names_transform = ~gsub('.',' ',.x,fixed = TRUE)) %>% 
        filter(value>0) %>% select(-value) }) %>% 
      bind_rows(.id='Vineyard') %>% 
      mutate(across(everything(),~ifelse(grepl('NA\\s?',.x),NA,.x))) %>% 
      write.csv(predictedBeesPath,row.names = FALSE)  
  }
  
  # Highlights list 1 ------------------------------------
  
  #Find unique plant/bee combinations for each vineyard
  
  #Step 1: unique combinations (easy)
  #Step 2: shared combinations (medium)
  #Step 3: no non-weedy native plants (fiddly)
  
  #Simple solution: choose rarer sets of bees/plants first, then descend
  #Vineyards with rarer plants also get rarer bees (?)
  
  #Turns counts into a 3D array - bees, plants, vineyards
  vyCounts <- lapply(vyNetworks,function(x){ #Get list and counts of bee/plant combos 
    x <- x$ntwk_noWeed
    if(!any(class(x)=='matrix')){
      d <- data.frame(bee=NA,plant=NA)
    } else {
      ind <- which(x>0,arr.ind = TRUE)
      d <- data.frame(bee=colnames(x)[ind[,'col']],plant=rownames(x)[ind[,'row']])  
    }
    return(d)
  }) %>% bind_rows(.id = 'vineyard') %>% 
    mutate(across(everything(),factor)) %>% 
    relocate(bee,plant,vineyard) %>% table() %>% 
    as.array()>0
  
  #Reorder array (most-to-least bee richness)
  vyCounts <- vyCounts[order(marginSums(vyCounts,1),decreasing = TRUE),
                       order(marginSums(vyCounts,2),decreasing = TRUE),
                       order(marginSums(vyCounts,3),decreasing = TRUE)]
  # image(apply(vyCounts,c(1,2),any)) #Presence/absence matrix across all vineyards
  
  #Sequential approach - rarest to most common
  
  #Storage dataframe for combinations
  vyCombos <- data.frame('vineyard'=names(vyCounts[1,1,]),
                         'bee'=rep(NA,dim(vyCounts)[3]),'plant'=rep(NA,dim(vyCounts)[3]))
  vyc <- vyCounts[!grepl('Apis',names(vyCounts[,1,1])),,] #Copy of vyCounts (removes honeybees)
  vyCombos[marginSums(vyc,3)==0,c(2,3)] <- 'All weeds' #Vineyards with no non-weedy plants
  vyc <- vyc[marginSums(vyc,1)>0,marginSums(vyc,2)>0,marginSums(vyc,3)>0,drop=FALSE] #Remove empty rows/cols/vineyards
  
  # # Sequential approach by bee - 2 unmatched
  # while(any(is.na(vyCombos$bee))){
  #   rBee <- rownames(vyc)[nrow(vyc)] #Rarest bee
  #   loc <- which(vyc[nrow(vyc),,],arr.ind = TRUE) #Array indices of plant/vineyard for last bee
  #   if(is.null(dim(loc))){
  #     loc <- c(loc,1)
  #     } else if(nrow(loc)>1){
  #       loc <- loc[which.max(loc[,2]),] #Chooses last vineyard (lowest diversity)
  #     }
  #   #Write to storage
  #   vyCombos$bee[vyCombos$vineyard==dimnames(vyc)$vineyard[loc[2]]] <- rBee
  #   vyCombos$plant[vyCombos$vineyard==dimnames(vyc)$vineyard[loc[2]]] <- colnames(vyc)[loc[1]]
  #   vyc <- vyc[-nrow(vyc),-loc[1],-loc[2],drop=FALSE] #Remove matched records/vineyards
  #   vyc <- vyc[marginSums(vyc,1)>0,marginSums(vyc,2)>0,marginSums(vyc,3)>0,drop=FALSE] #Remove empty rows/cols/vineyards
  # 
  #   if(length(vyc)==0&any(is.na(vyCombos$bee))){
  #     warning(paste0('Ran out of bee/plant combinations. ',sum(is.na(vyCombos$bee)),' unmatched vineyards remain:\n',
  #                    paste(vyCombos$vineyard[is.na(vyCombos$bee)],collapse = '\n')))
  #     break()
  #   }
  # }
  #
  #
  # # "Loneliest partners" first (bee-plant with lowest # of secondary connections) - 3 unmatched
  # while(any(is.na(vyCombos$bee))){
  #   sc <- secConn(apply(vyc,c(1,2),any)) #Matrix of secondary connections
  #   sc <- which(sc==min(sc[sc>=0]),arr.ind = TRUE) #Minimum nonzero secondary connections
  #   sc_min <- which.max(sqrt(sc[,1]^2+sc[,2]^2)) #Chooses furthest distance away (rarest bee/plant)
  # 
  #   loc <- sc[sc_min,] #Array indices of last (rarest) bee/plant
  #   rVy <- which(vyc[loc[1],loc[2],])[max(sum(vyc[loc[1],loc[2],]))] #Gets vineyard (last in list)
  # 
  #     #Write to storage
  #   vyCombos$bee[vyCombos$vineyard==names(rVy)] <- rownames(vyc)[loc[1]] #Rarest bee with fewest connections
  #   vyCombos$plant[vyCombos$vineyard==names(rVy)] <- colnames(vyc)[loc[2]]
  #   vyc <- vyc[-loc[1],-loc[2],-rVy,drop=FALSE] #Remove matched records/vineyards
  #   vyc <- vyc[marginSums(vyc,1)>0,marginSums(vyc,2)>0,marginSums(vyc,3)>0,drop=FALSE] #Remove empty rows/cols/vineyards
  # 
  #   if(length(vyc)==0&any(is.na(vyCombos$bee))){
  #     warning(paste0('Ran out of bee/plant combinations. ',sum(is.na(vyCombos$bee)),' unmatched vineyards remain:\n',
  #                    paste(vyCombos$vineyard[is.na(vyCombos$bee)],collapse = '\n')))
  #     break()
  #   }
  # }
  #
  # #Random assignment - can't get better than 3 unmatched
  # {a <- Sys.time()
  # rReps <- replicate(10000,{
  #   #Storage dataframe for combinations
  #   vyCombos <- data.frame('vineyard'=names(vyCounts[1,1,]),
  #                          'bee'=rep(NA,dim(vyCounts)[3]),'plant'=rep(NA,dim(vyCounts)[3]))
  #   vyc <- vyCounts #Copy of vyCounts
  #   
  #   vyCombos[marginSums(vyc,3)==0,c(2,3)] <- 'None' #Vineyards with no non-weedy plants
  #   vyc <- vyc[,,!marginSums(vyc,3)==0]
  #   
  #   while(any(is.na(vyCombos$bee))){
  #     loc <- which(vyc,arr.ind = TRUE)[sample(1:sum(vyc),1),] #Array indices of random bee/plant/vineyard
  #     rVy <- dimnames(vyc)$vineyard[loc[3]] #Vineyard name
  #     
  #     #Write to storage
  #     vyCombos$bee[vyCombos$vineyard==rVy] <- rownames(vyc)[loc[1]] #Rarest bee with fewest connections
  #     vyCombos$plant[vyCombos$vineyard==rVy] <- colnames(vyc)[loc[2]]
  #     vyc <- vyc[-loc[1],-loc[2],-loc[3],drop=FALSE] #Remove matched records/vineyards
  #     vyc <- vyc[marginSums(vyc,1)>0,marginSums(vyc,2)>0,marginSums(vyc,3)>0,drop=FALSE] #Remove empty rows/cols/vineyards
  #     
  #     if(length(vyc)==0) break()
  #   }
  #   
  #   if(length(vyc)==0&any(is.na(vyCombos$bee))){
  #     return(vyCombos$vineyard[is.na(vyCombos$bee)])
  #   } else {
  #     return(vyCombos)
  #   }  
  # })
  # b <- Sys.time()}
  # 
  # table(sapply(rReps,mode))
  # table(sapply(rReps,length))
  
  #Sequential approach by vineyard - works!
  while(any(is.na(vyCombos$bee))){
    
    rVy <- vyCombos$vineyard[is.na(vyCombos$bee)][sum(is.na(vyCombos$bee))] #Vineyard to use (lowest-diversity), otherwise no matches appear
    
    loc <- which(vyc[,,dim(vyc)[3]],arr.ind = TRUE) #Get combos from last (lowest-diversity) vineyard 
    loc <- loc[which.max(apply(loc,1,function(x) sqrt(sum(x^2)))),] #Chooses rarest bee/plant combo
    
    #Write to storage
    vyCombos$bee[vyCombos$vineyard==rVy] <- rownames(vyc)[loc[1]] 
    vyCombos$plant[vyCombos$vineyard==rVy] <- colnames(vyc)[loc[2]]
    vyc <- vyc[-loc[1],-loc[2],!names(vyc[1,1,])==rVy,drop=FALSE] 
    vyc <- vyc[marginSums(vyc,1)>0,marginSums(vyc,2)>0,marginSums(vyc,3)>0,drop=FALSE] #Remove empty rows/cols/vineyards
    
    if(length(vyc)==0&any(is.na(vyCombos$bee))){
      warning(paste0('Ran out of bee/plant combinations. ',sum(is.na(vyCombos$bee)),' unmatched vineyards remain:\n',
                     paste(vyCombos$vineyard[is.na(vyCombos$bee)],collapse = '\n')))
      break()
    }
  }
  
  # vyCombos #Unique records - "Highlights"
  
  #Join bee and plant abstracts
  beeAbstracts <- read.csv(beeAbstractsPath) %>% 
    mutate(across(everything(),~trimws(gsub('  ',' ',.x)))) #Get rid of extra whitespace and double spaces
  
  #Join bee/plant abstracts on to unique bees/plants
  vyCombos <- vyCombos %>% left_join(beeAbstracts,by=c('bee'='Bee_Scientific_Name')) %>% 
    left_join(transmute(plantList,Scientific_name,Plant_Common_Name=Common_name,Plant_Abstract=PlantAbstract),
              by=c('plant'='Scientific_name')) 
  # %>% write.csv(beePlantPairsPath,row.names = FALSE) #Writes to file
  
  # Highlights list 2 ----------------------------------------------
  
  #Chooses bee/plant highlight lists independently (highlighted bees don't need to visit highlighted plants)
  # vyNetworks[[1]] %>% str
  
  #Storage dataframe
  vyCombos2 <- data.frame('vineyard'=names(vyCounts[1,1,]),
                          'bee'=rep(NA,dim(vyCounts)[3]),'plant'=rep(NA,dim(vyCounts)[3]))
  vyCombos2[vyCombos2$vineyard %in% names(vyNetworks)[sapply(vyNetworks,function(x) any(is.na(x$ntwk_noWeed)))],
            c('bee','plant')] <- 'All weeds' #Vineyards with no non-weedy plants
  
  #Vineyard/plant matrix
  vyp <- sapply(vyNetworks,function(x) data.frame(plants=rownames(x$ntwk_noWeed))) %>% #Non-weedy plants from each vineyards
    bind_rows(.id='vineyard') %>% table() %>% as.array()>0
  # Reorder matrix
  vyp <- vyp[order(rowSums(vyp),decreasing = FALSE),] #Vineyards, from least spp-rich to least
  vyp <- vyp[,order(colSums(vyp),decreasing = TRUE)] #Plants, from most common to least
  #Vineyard/bee matrix
  vyb <- sapply(vyNetworks,function(x) data.frame(bees=colnames(x$ntwk_noWeed))) %>% #Bees from each vineyards
    bind_rows(.id='vineyard') %>% filter(bees!='Apis mellifera') %>% table() %>% as.array()>0
  # Reorder matrix
  vyb <- vyb[order(rowSums(vyb),decreasing = FALSE),] #Vineyards, from most spp-rich to least
  vyb <- vyb[,order(colSums(vyb),decreasing = TRUE)] #Bees, from least common to most
  
  while(any(is.na(vyCombos2$plant))){
    loc <- which(vyp,arr.ind = TRUE) #Get combos from last (lowest-diversity) vineyard
    loc <- loc[which.max(apply(loc,1,function(x) sqrt(sum(x^2)))),] #Chooses lowest diversity/most common plant combo
    
    vyCombos2[vyCombos2$vineyard==rownames(vyp)[loc[1]],'plant'] <- colnames(vyp)[loc[2]]
    vyp <- vyp[-loc[1],-loc[2],drop=FALSE] #Remove vineyard/plant combo
    vyp <- vyp[rowSums(vyp)>0,colSums(vyp)>0,drop=FALSE] #Cleanup empty plants/vineyards
    
    if(length(vyp)==0&any(is.na(vyCombos2$bee))){
      warning(paste0('Ran out of plants. ',sum(is.na(vyCombos2$plant)),' unmatched vineyards remain:\n',
                     paste(vyCombos2$vineyard[is.na(vyCombos2$plant)],collapse = '\n')))
      vyCombos2$plant[is.na(vyCombos2$plant)] <- 'No matches'
      break()
    }
  }
  
  while(any(is.na(vyCombos2$bee))){
    loc <- which(vyb,arr.ind = TRUE) #Get combos from last (lowest-diversity) vineyard
    loc <- loc[which.max(apply(loc,1,function(x) sqrt(sum(x^2)))),] #Chooses lowest diversity/most common bee combo
    
    vyCombos2[vyCombos2$vineyard==rownames(vyb)[loc[1]],'bee'] <- colnames(vyb)[loc[2]]
    vyb <- vyb[-loc[1],-loc[2],drop=FALSE] #Remove vineyard/bee combo
    vyb <- vyb[rowSums(vyb)>0,colSums(vyb)>0,drop=FALSE] #Cleanup empty bees/vineyards
    
    if(length(vyb)==0&any(is.na(vyCombos2$bee))){
      warning(paste0('Ran out of bees. ',sum(is.na(vyCombos2$bee)),' unmatched vineyards remain:\n',
                     paste(vyCombos2$vineyard[is.na(vyCombos2$bee)],collapse = '\n')))
      vyCombos2$bee[is.na(vyCombos2$bee)] <- 'No matches'
      break()
    }
  }
  
  rm(vyc,vyb,vyp,loc,rVy)
  
  #Join bee/plant abstracts on to unique bees/plants
  vyCombos2 <- vyCombos2 %>% left_join(beeAbstracts,by=c('bee'='Bee_Scientific_Name')) %>% 
    left_join(transmute(plantList,Scientific_name,Plant_Common_Name=Common_name,Plant_Abstract=PlantAbstract),
              by=c('plant'='Scientific_name')) 
  # %>% write.csv(beePlantPairsPath2,row.names = FALSE) #Writes to csv
  
  # save.image(dataStoragePath)
  
  # Create reports --------------------
  print(paste0('Creating reports (',length(vyNetworks),' total)'))

  #Debugging  
    render('./R/vineyard-report-template.Rmd',
           params = list(set_title="Abbott_Claim"),
           intermediates_dir = './reports',
           output_dir = './reports',knit_root_dir = NULL,
           output_file = 'Abbott_Claim-report.pdf',
           envir=parent.frame(),
           clean=FALSE
    )
  
  # for(i in 1:length(names(vyNetworks))){
  #   render('vineyard-report-template.Rmd',
  #          params = list(set_title=names(vyNetworks)[i]),
  #          intermediates_dir = './reports',
  #          output_dir = './reports',knit_root_dir = NULL,
  #          output_file = paste0(names(vyNetworks)[i],'-report.pdf'),
  #          envir=parent.frame(),
  #          clean=TRUE
  #   )
  #   print(paste0('Finished report ',names(vyNetworks)[i]))
  # }
}




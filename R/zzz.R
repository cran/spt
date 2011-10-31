.onLoad <- function(lib, pkg){
  packageStartupMessage("spt 1.1 loaded ...")
  assign('.sptConnect',NULL,pos=.GlobalEnv) 
}

.onUnload <- function(libpath)
    library.dynam.unload("spt",  libpath)

.sptConnect <- NULL


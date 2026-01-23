

# very short script to check if required packages are installed, and to install them if not. 
# unlikely that it will matter what version of these packages are used, because they're quite robust with respect to the main functions used. 
# however, it would still be good practice to do so, and some of them still might change. So a to do is to install specific versions. 


## 1. put the names of the packages to be installed into a list
packages_to_install <- list("nomisr", "data.table", "parallel", "openxlsx", "mipfp", "reshape2", "foreach", "doParallel", "devtools")


## 2. define the function that will check if the package has been installed, and will install the package if it has not been
check_and_install <- function(package_ins){
  
  if(!(nzchar(system.file(package = package_ins)))){
    
    install.packages(package_ins)
    
  }
  
}


## 3. apply the function over the list of package names

lapply(
  X = packages_to_install,
  FUN = check_and_install
)


## 4. separate section needed for gsscoder. It's not a CRAN package, so a different process is needed to install

if(!nzchar(system.file(package = "gsscoder"))){
  
  devtools::install_github("Greater-London-Authority/gsscoder")
  
  
}

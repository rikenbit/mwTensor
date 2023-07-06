# Base Image
FROM bioconductor/bioconductor_docker:RELEASE_3_17

# Install R Packages
RUN R -e "BiocManager::install(version = '3.17', ask=FALSE); BiocManager::valid(); BiocManager::install('mixOmics'); devtools::install_github('rikenbit/mwTensor', \
    upgrade='always', force=TRUE, INSTALL_opts = '--install-tests');\
    tools::testInstalledPackage('mwTensor')"

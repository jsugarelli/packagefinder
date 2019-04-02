# packagefinder 0.1.0

With version 0.1.0 the features of packagefinder have been significantly enhanced.


## Major changes

* New function exploreFields() allows the user to search for packages by looking into specific fields of the package's information from CRAN.

* New function packageDetails() shows detailed information for any CRAN package.

* New function whatsNew() shows packages recently uploaded to CRAN.

* New function lastResults() reproduces the results of the users' last package search with findPackage().


## Minor changes

* packagefinder functions now use colored output in the RStudio console (feature only available when using RStudio).

* Startup message on loading the package reminds the user briefly of how to use packagefinder.

* Download column in search results is made invisible if search index does not include download figures.

* Function findPackage() now supports an argument 'only.top' to limit the number of search results presented (default: 15).

* Function findPackage()'s argument 'index' has been moved to third place in the order of arguments (since this optional argument is often left out when calling findPackage()).

* Function go() now supports "details" as value for argument 'where.to' (shows details of the package); this value is the new default for 'where.to'.

* The option "cran" in function go()'s argument 'where.to' has been renamed to "website" in order to make it more general (as in many cases, the website is actually a GitHub page or custom website)

* Function findPackage() now supports an argument 'silent' (default: FALSE); it controls if details of the user's search request are printed as part of the search results.

* Function go()'s argument 'package' can now also be the package's name as a string (instead of the 'GO' index number presented in the search results).

* Function buildIndex() now incorporates a significant larger number of CRAN fields into the search index than before, to enable functionalities provided by packageDetails() and exploreFields().

* Package tools (>= 3.4.0) is now required to make sure function CRAN__package__db() is available to packagefinder (GitHub issue #3, @SteffenMoritz).

* README.md has been significantly expanded to provide a comprehensive introduction to working with the packagefinder package.


## Bug fixes

* Publishing date of the CRAN packages was not retrieved correctly. Fixed.


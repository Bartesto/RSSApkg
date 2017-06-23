# RSSApkg 1.0.0

##Major Release:
The `RSSApkg` combines a number of internally used functions in the RSSA section 
of Parks and Wildlife (Western Australia). These will be added to over time as 
newly developed functions mature. They will be available in new minor releases.

###Changes:

* All old functions involved with QA purpose have had a fresh update with a view
to longeivity and generalisation.

* `jpegR` is now faster and has options for date filtering to enable a more
focused process. It also exports a txt file containg name of original shape file
that was split for processing.

* `extractR` output collated csv now restricted to min and max of extracted 
values.

* Utility functions `u_dateR`, `u_leapR` and `u_shpsplitR` which are used 
internally in jpegR and extractR are now available for use.




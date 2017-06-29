# RSSApkg 1.0.0

* Initial release of RSSApkg.
* Reworking of `jpegR` to:
    + enable begining and end dates to target analysis period.
    + output a small text file with name of original shape file.
    + split original shape file and store in new folder named `site_vectors`.
    + clean code and improve speed.
* Reworking of `extractR` to:
    + restrict min and max of dates in csv output to those utilised.
    + clean code and improve speed.
* Access to internal helper functions now grouped as utility functions and 
includes `u_dateR`, `u_leapR` and `u_shpsplitR`.




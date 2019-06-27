# asesgeo

This package contains frequently used functions on geographic for ASES, regarding coordinate coversion, IP/address parsing, etc.

This package is comprised of 
- Coordinate conversion
    - Local tranformer: wgs2gcj(), wgs2bd(), gcj2wgs(), gcj2bd(), bd2wgs(), bd2gcj()
    - Generic wrapper: conv_coord()

- Geohost (ip parser)
    - geohost() or parse_geohost()

- Geocode (address locator)
    - geocode() or parse_geocode()

- Reverse geocode (coordinate locator)
    - revgeocode() or parse_revgeocode()

- Helpers
    - Formatter: coord_format, lat_coord(), lon_coord()
    - Inside or outside China: is_out_of_china()
    - Show my IP: show_my_ip()
    
    
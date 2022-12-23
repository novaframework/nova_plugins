### nova_correlation_plugin
**Version**: 0.1.0
**Author**: Nova team <info@novaframework.org
**Description**:
Add X-Correlation-ID headers to response

### nova_cors_plugin
**Version**: 0.2.0
**Author**: Nova team <info@novaframework.org
**Description**:
Add CORS headers to request
| Parameter | Description |
| --- | --- |
| allow_origins | Specifies which origins to insert into Access-Control-Allow-Origin |


### Nova body plugin
**Version**: 0.0.1
**Author**: Nova team <info@novaframework.org
**Description**:
This plugin modulates the body of a request.
| Parameter | Description |
| --- | --- |
| parse_bindings | Used to parse bindings and put them in state under `bindings` key |
| decode_json_body | Decodes the body as JSON and puts it under `json` |
| parse_qs | Used to parse qs and put hem in state under `qs` key |



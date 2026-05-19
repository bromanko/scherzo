import scherzo/tracker/conformance/json
import scherzo/tracker/conformance/types

pub const schema_version = types.schema_version

pub fn decode_manifest(
  contents: String,
) -> Result(types.Manifest, types.ManifestError) {
  json.decode_manifest(contents)
}

pub fn decode_request(contents: String) -> Result(types.DriverRequest, Nil) {
  json.decode_request(contents)
}

pub fn decode_response(contents: String) -> Result(types.DriverResponse, Nil) {
  json.decode_response(contents)
}

pub fn manifest_to_string(manifest: types.Manifest) -> String {
  json.manifest_to_string(manifest)
}

pub fn request_to_string(request: types.DriverRequest) -> String {
  json.request_to_string(request)
}

pub fn response_to_string(response: types.DriverResponse) -> String {
  json.response_to_string(response)
}

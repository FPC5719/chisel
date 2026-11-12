#!/usr/bin/env bash

set -euo pipefail

PLUGIN_SCALA_VERSION="2.13.18"
UNIPUBLISH_SCALA_VERSION="2.13"
MODULES="plugin.cross[${PLUGIN_SCALA_VERSION}],unipublish[${UNIPUBLISH_SCALA_VERSION}]"

for required_env in MILL_SONATYPE_USERNAME MILL_SONATYPE_PASSWORD MILL_PGP_SECRET_BASE64; do
  if [[ -z "${!required_env:-}" ]]; then
    echo "error: ${required_env} must be set" >&2
    echo "note: Mill's dry-run still imports the signing key" >&2
    exit 1
  fi
done

VERSION=$(
  ./mill --no-server show "unipublish[${UNIPUBLISH_SCALA_VERSION}].publishVersion" |
    tr -d '"'
)
METADATA=$(
  ./mill --no-server show "unipublish[${UNIPUBLISH_SCALA_VERSION}].artifactMetadata"
)
BUNDLE_NAME=$(jq -r '.group + "." + .id + "-" + .version' <<< "${METADATA}")
IS_SNAPSHOT=$(
  ./mill --no-server show v.isSnapshot |
    tr -d '"'
)

BUNDLE_NAME_ARGS=()
if [[ "${IS_SNAPSHOT}" != "true" ]]; then
  BUNDLE_NAME_ARGS=(--bundleName "${BUNDLE_NAME}")
fi

echo "Publishing ${MODULES} as version ${VERSION}"

./mill --no-server mill.javalib.SonatypeCentralPublishModule/ \
  --shouldRelease "false" \
  "${BUNDLE_NAME_ARGS[@]}" \
  --publishArtifacts "{${MODULES}}.publishArtifacts"

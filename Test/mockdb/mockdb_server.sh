#! /usr/bin/env zsh

require() { { hash npm && hash curl } || exit 127 }

if (( $# )); then
  require
  BASEURL="$1"
  CATEGORIES="$2"
  mkdir -p mockdb.tmp.d/{products.d,avail.d}
  cd mockdb.tmp.d

  printf "Fetching remote resources from /products ... "
  curl -sO --output-dir products.d \
    "${BASEURL}/products/{${CATEGORIES}}" || exit "$?"
  printf "done!\n"

  printf "Concatenating resources from /products to mockdb.json ... "
  printf "{" > mockdb.json
  cd products.d
  for jsonfile in * ; do
    printf '"%s":' "$jsonfile" | cat - "$jsonfile" >> ../mockdb.json
    printf "," >> ../mockdb.json
  done
  cd ..
  printf "done!\n"
  
  printf "Parsing products.json for unique manufacturer names ... "
  manufacturers=($(
    grep -oE '"manufacturer":"[^"]+"' mockdb.json \
    | cut -c 17- \
    | sort -u \
    | tr -s '"\n' ' ' \
  ))
  printf "done!\n"

  printf "Fetching remote resources from /availability ..."
  for mf in "${manufacturers[@]}" ; do
    curl -sO -D avail.d/head.dump --output-dir avail.d \
      "${BASEURL}/availability/${mf}" || exit "$?"
    while grep -q 'availability-empty' avail.d/head.dump ; do
      printf 'error for %s!\nTrying again ... ' "$mf"
      rm avail.d/head.dump avail.d/"$mf"
      curl -sO -D avail.d/head.dump --output-dir avail.d \
        "${BASEURL}/availability/{${mf}}" || exit "$?"
      done
    rm avail.d/head.dump
  done
  printf "done!\n"

  printf "Concatenating resources from /availability to mockdb.json ... "
  cd avail.d
  for jsonfile in * ; do
    printf '"%s":' "$jsonfile" | cat - "$jsonfile" >> ../mockdb.json
    printf "," >> ../mockdb.json
  done
  cd ..
  # substitute a right brace for the final comma:
  sed -i '$s/,$/}/' mockdb.json 
  printf "done!\n"

  printf "Backing up and cleaning ..."
  cd ..
  ! [[ -f mockdb.json ]] || rename json json.bak mockdb.json.bak
  mv mockdb.tmp.d/mockdb.json .
  rm -rf mockdb.tmp.d
  printf "done!\n"
fi

if ! npm list --depth 1 --global json-server > /dev/null 2>&1 ; then
  printf "json-server not found; installing as a global npm package.\n"  
  npm install --global json-server > /dev/null 2>&1 || {  
    printf "Something went wrong installing json-server.\n" && exit 1
  }
  printf "Success! Trying to update your PATH via env.\n"
  env > /dev/null 2>&1 || exit 1
fi

if [[ -f mockdb.json ]] ; then 
  printf "Starting json-server now...\n"
  json-server --middlewares middlewares.js --routes routes.json --watch mockdb.json
  exit 0
fi
printf "mockdb.json does not exist; please fetch the data first.\n"
exit 1

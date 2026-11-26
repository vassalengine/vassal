#!/bin/bash -e

url=https://vassalengine.org/api/gls/v1/projects
query='?limit=100'

while [[ "x$query" != "xnull" ]] ; do
  r=$(curl -s "${url}${query}" | jq -rc '.meta.next_page, .projects[]')
  query=$(echo "$r" | head -1)
  echo "$r" | tail -n +2
  echo -n . >&2
done | jq -s '[ .[] | {name, slug, title: .game.title, sort: .game.title_sort_key, modified_at} ]'

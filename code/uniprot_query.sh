# Code to query Uniprot webservice with a .csv file
# In this case it takes the first column of the .csv file ignoring the headers
# The terms to compose the query can be found here:
# https://www.uniprot.org/help/return_fields

curl -H "Accept: text/plain; format=tsv" \
'ids=$(cut -f1 -d',' plasma_protein_annotation.csv | tail -n +2)' \
 "https://rest.uniprot.org/uniprotkb/search?query=human&fields=accession,xref_proteomes,go" > uniprot_query.tsv
# Import required libraries and packages
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)
from pandas.io.json import json_normalize as json_normalize
from tqdm import tqdm
import pandas as pd
import requests
import argparse
import json
import os
import shutil
import sys
import re

# gnomAD API
end_point = "https://gnomad.broadinstitute.org/api/"

# Argument Parsing
def arg_parser():
    global gene
    parser = argparse.ArgumentParser()
    parser.add_argument("-gene", type=str, required=True, default="UBE3A", help="The gene you wish to find missense variants for (Like UBE3A)")
    args = parser.parse_args()
    
    gene = args.gene
    return gene

# Main Function
def get_variants_by(gene):
    post = {"query": "query Search($dataset: DatasetId!, $query: String!) {  searchResults(dataset: $dataset, query: $query) {    label    value: url  }}","variables": {"dataset": "gnomad_r2_1","query": gene}}

    # Get repsonse
    response = requests.post(end_point, json=post, timeout=None)
    if response.status_code == 200:
        res = response.json()
        label = res["data"]["searchResults"][0]["label"]
        gene_id = res["data"]["searchResults"][0]["value"]
        gene_id = re.search("([A-Z])\w+",gene_id).group()
        print(gene_id)
        
        search_by_id = {"query": "\nquery VariantsInGene($geneId: String!, $datasetId: DatasetId!, $referenceGenome: ReferenceGenomeId!) {\n   gene(gene_id: $geneId, reference_genome: $referenceGenome) {\n      variants(dataset: $datasetId) {\n    chrom\n      pos\n    rsid\n    ref\n   alt\n      consequence\n      hgvs\n      hgvsc\n      hgvsp\n      lof\n      lof_filter\n      lof_flags\n       transcript_id\n      variant_id\n      exome {\n        ac\n        ac_hemi\n        ac_hom\n        an\n        af\n        filters\n      }\n     }\n  }\n}","variables": {"datasetId": "gnomad_r2_1","geneId": gene_id,"referenceGenome": "GRCh37"}}

        response = requests.post(end_point, json=search_by_id, timeout=None)
        print(response)

        json_keys = list(response.json()["data"]["gene"].keys())
        for json_key in json_keys:
            # print(json_key, type(response.json()["data"]["gene"][json_key]), response.json()["data"]["gene"][json_key] is None, type(response.json()["data"]["gene"][json_key]) not in [str, int])
            data = json_normalize(response.json()["data"]["gene"][json_key])
            data.columns = data.columns.map(lambda x: x.split(".")[-1])
            #Get only missense mutations
            data = data[data.consequence == 'missense_variant']
            #Rename columns to match gnomad format
            data.rename(columns={'hgvsp':'Protein Consequence'}, inplace=True)
            data.rename(columns={'pos':'Position'}, inplace=True)
            data.rename(columns={'rsid':'rsID'}, inplace=True)
            data.rename(columns={'af':'Allele Frequency'}, inplace=True)

            #data.to_csv("outputs/" + label + "/" + label +"_"+ json_key + ".csv", sep=",", index=False)
            data.to_csv("temp/" + label + ".csv", sep=",", index=False)

    elif response.status_code == 404:
        sys.exit('API is not accessible right now. Check the end point out!')
    
# Action
if __name__ == "__main__":
    gene = arg_parser()    
    if "." in gene:
        try:
            with open(gene, "r") as f:
                search_list = [line.rstrip() for line in f]
                print(search_list)
                for search_item in tqdm(search_list):
                    get_variants_by(search_item)
        except:
            print("A problem occured while reading the file namely `{}`"\
                  .format(gene))
        finally:
            f.close()
    elif "." not in gene:
        get_variants_by(gene)
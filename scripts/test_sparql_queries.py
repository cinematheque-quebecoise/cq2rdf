import argparse
import sys, os
from os import path
import yaml
import gzip
from rdflib import Graph
from rdflib_hdt import HDTStore, optimize_sparql
from tabulate import tabulate
from SPARQLWrapper import SPARQLWrapper, JSON

def test_query(sparql_endpoint, query_name, query):
    print(f'Testing query "{query_name}" ... ', end='')

    sparql = SPARQLWrapper(sparql_endpoint)

    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    results = sparql.query().convert()['results']['bindings']

    if len(results) == 0:
        print('[FAILED] 0 results')
    else:
        print(f'[OK] {len(results)} results')
        print('First 5 rows:')
        print(tabulate([[v['value'] for v in r.values()] for r in results][:5]))

def main(args):
    query_index_fpath = path.join(args.querydir, 'index.yaml')

    with open(query_index_fpath) as yamlFile:
        queries_index = yaml.load(yamlFile, Loader=yaml.FullLoader)
        for query_index in queries_index:
            test_query(args.endpoint, query_index['name'], query_index['query'])

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Test SPARQL queries on dataset.')
    parser.add_argument('-e', dest='endpoint', help='SPARQL endpoint', required=True)
    parser.add_argument('-q', dest='querydir', help='directory containing SPARQL queries', required=True)

    args = parser.parse_args()
    main(args)

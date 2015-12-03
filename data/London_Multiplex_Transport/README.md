

# LONDON MULTIPLEX TRANSPORTATION NETWORK

###### Last update: 1 July 2014

### Reference and Acknowledgments

This README file accompanies the dataset representing the multiplex transportation network of London (UK). 
If you use this dataset in your work either for analysis or for visualization, you should acknowledge/cite the following paper:

	“Navigability of interconnected networks under random failures”
	Manlio De Domenico, Albert Solé-Ribalta, Sergio Gómez, and Alex Arenas
	PNAS 2014 111 (23) 8351-8356

that can be found at the following URL:

<http://www.pnas.org/content/111/23/8351.abstract>

This work has been supported by European Commission FET-Proactive project PLEXMATH (Grant No. 317614), the European project devoted to the investigation of multi-level complex systems and has been developed at the Alephsys Lab. 

Visit

PLEXMATH: <http://www.plexmath.eu/>

ALEPHSYS: <http://deim.urv.cat/~alephsys/>

for further details.



### Description of the dataset

Data was collected in 2013 from the official website of Transport for London (<https://www.tfl.gov.uk/>) and manually cross-checked.

Nodes are train stations in London and edges encode existing routes between stations. Underground, Overground and DLR stations are considered (see <https://www.tfl.gov.uk/> for further details).
The multiplex network used in the paper makes use of three layers corresponding to:

1.	The aggregation to a single weighted graph of the networks of stations corresponding to each underground line (e.g., District, Circle, etc)
2.	The network of stations connected by Overground
3.	The network of stations connected by DLR

There are 369 nodes in total, labelled with integer ID between 0 and 368.
The multiplex is undirected (with only one direction specified) and weighted, stored as edges list in the file
    
    london_transport_multiplex.edges

with format

    layerID nodeID nodeID weight

The IDs of all layers are stored in 

    london_transport_layers.txt

The IDs of nodes, together with their geographical coordinates (latitude and longitude in deg) can be found in the file

    london_transport_nodes.txt

For completeness, we provide in the file

    london_transport_raw.edges

with format 

    Line Station Station

the raw list of connections between stations specifying the Tube line in plain text. This should facilitate the construction of a multiplex network with up to 13 layers (11 Underground lines + Overground + DLR)



### Real disruptions in London transport As part of the study, we have collected information about real disruptions (“no “service” status) in London transport  from Twitter (see the paper and its suppl. inf. for further details).Here, we provide the disrupted multiplex networks used in the study, i.e., the multiplex networks constructed by accounting for specific disruptions. In file
    london_transport_disruptions_summary.txt

we list the disruptions considered, providing the following details:

    DisruptionID Line Station_A Station_B Frequency(in %) Affected_nodes(in %)%

whereas in the folder 

    Disruptions

we stored the multiplex networks in the same format as described above. The name of each file is
 
    london_transport_multiplex_XXX.edges

where the disruption ID is placed instead of XXX.


### License

This LONDON MULTIPLEX TRANSPORTATION NETWORK DATASET is made available under the Open Database License: <http://opendatacommons.org/licenses/odbl/1.0/>. Any rights in individual contents of the database are licensed under the Database Contents License: <http://opendatacommons.org/licenses/dbcl/1.0/>

You should find a copy of the above licenses accompanying this dataset. If it is not the case, please contact us (see below).

A friendly summary of this license can be found here:

<http://opendatacommons.org/licenses/odbl/summary/>

and is reported in the following.

======================================================
ODC Open Database License (ODbL) Summary

This is a human-readable summary of the ODbL 1.0 license. Please see the disclaimer below.

You are free:

*    To Share: To copy, distribute and use the database.
*    To Create: To produce works from the database.
*    To Adapt: To modify, transform and build upon the database.

As long as you:
    
*	Attribute: You must attribute any public use of the database, or works produced from the database, in the manner specified in the ODbL. For any use or redistribution of the database, or works produced from it, you must make clear to others the license of the database and keep intact any notices on the original database.
    
*	Share-Alike: If you publicly use any adapted version of this database, or works produced from an adapted database, you must also offer that adapted database under the ODbL.
    
*	Keep open: If you redistribute the database, or an adapted version of it, then you may use technological measures that restrict the work (such as DRM) as long as you also redistribute a version without such measures.

======================================================


### Contacts

If you find any error in the dataset or you have questions, please contact

	Manlio De Domenico
	Universitat Rovira i Virgili 
	Tarragona (Spain)

email: <manlio.dedomenico@urv.cat>web: <http://deim.urv.cat/~manlio.dedomenico/>
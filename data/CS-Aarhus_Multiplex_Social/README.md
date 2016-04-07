

#  MULTIPLEX NETWORK

###### Last update: 1 July 2014

### Reference and Acknowledgments

This README file accompanies the dataset representing the multiplex social network of a research department at Aarhus.
If you use this dataset in your work either for analysis or for visualization, you should acknowledge/cite the following paper:
	
	“Combinatorial Analysis of Multiple Networks”
	Matteo Magnani, Barbora Micenkova, and Luca Rossi
	arXiv:1303.4986 (2013)

that can be found at the following URL:

<http://arxiv.org/abs/1303.4986>

See the official webpage of this dataset

<http://larica.uniurb.it/sigsna/data/>

for further details.



### Description of the dataset

The multiplex social network consists of five kinds of online and offline relationships (Facebook, Leisure, Work, Co-authorship, Lunch) between the employees of Computer Science department at Aarhus.

There are 61 nodes in total, labelled with integer ID between 1 and 61, with 620 connections.
The multiplex is undirected (with only one direction specified) and unweighted, stored as edges list in the file
    
    CS-Aarhus_multiplex.edges

with format

    layerID nodeID nodeID weight

(Note: all weights are set to 1)

The IDs of all layers are stored in 

    CS-Aarhus_layers.txt

The IDs of nodes can be found in the file

    CS-Aarhus_nodes.txt



### License

The CS-AARHUS MULTIPLEX DATASET is provided "as is" and without warranties as to performance or quality or any other warranties whether expressed or implied. 

For further information see <http://larica.uniurb.it/sigsna/data/>

### Contacts


<http://larica.uniurb.it/sigsna/data/>
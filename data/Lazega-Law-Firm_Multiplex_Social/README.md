

# LAZEGA-LAW-FIRM MULTIPLEX NETWORK

###### Last update: 1 July 2014

### Reference and Acknowledgments

This README file accompanies the dataset representing the multiplex social network of a corporate law partnership.
If you use this dataset in your work either for analysis or for visualization, you should acknowledge/cite the following papers:
	
	"The Collegial Phenomenon: The Social Mechanisms of Cooperation Among Peers in a Corporate Law Partnership"
	Emmanuel Lazega
	Oxford University Press (2001)

	"New specifications for exponential random graph models"
	Tom A.B. Snijders, Philippa E. Pattison, Garry L. Robins, and Mark S. Handcock
	Sociological Methodology (2006), 99-153.


### Description of the dataset

The multiplex social network consists of 3 kinds of (Co-work, Friendship and Advice) between partners and associates of a corporate law partnership.

There are 71 nodes in total, labelled with integer ID between 1 and 71, with 2223 connections.
The multiplex is directed and unweighted, stored as edges list in the file
    
    Lazega-Law-Firm_multiplex.edges

with format

    layerID nodeID nodeID weight

(Note: all weights are set to 1)

The IDs of all layers are stored in 

    Lazega-Law-Firm_layers.txt

The IDs of nodes can be found in the file

    Lazega-Law-Firm_nodes.txt

The values for the 8 columns (attributes) are:

1. node id
2. status (1=partner; 2=associate)
3. gender (1=man; 2=woman)
4. office (1=Boston; 2=Hartford; 3=Providence)
5. years with the firm
6. age
7. practice (1=litigation; 2=corporate)
8. law school (1: harvard, yale; 2: ucon; 3: other) 


### License

The LAZEGA-LAW-FIRM MULTIPLEX DATASET is provided "as is" and without warranties as to performance or quality or any other warranties whether expressed or implied. 




# CKM-PHYSICIANS-INNOVATION MULTIPLEX NETWORK

###### Last update: 1 July 2014

### Reference and Acknowledgments

This README file accompanies the dataset representing the multiplex social network of a sample of physicians in US.
If you use this dataset in your work either for analysis or for visualization, you should acknowledge/cite the following paper:

	"The Diffusion of an Innovation Among Physicians"
	J. Coleman, E. Katz, and H. Menzel. 
	Sociometry (1957) 20:253-270.


### Description of the dataset

Data collected by Coleman, Katz and Menzel on medical innovation, considering physicians in four towns in Illinois, Peoria, Bloomington, Quincy and Galesburg.

They were concerned with the impact of network ties on the physicians' adoption of a new drug, tetracycline. Three sociometric matrices (layers) were generated, based on the following questions:

1. When you need information or advice about questions of therapy where do you usually turn? 
2. And who are the three or four physicians with whom you most often find yourself discussing cases or therapy in the course of an ordinary week -- last week for instance?
3. Would you tell me the first names of your three friends whom you see most often socially?

There are 246 nodes in total, labelled with integer ID between 1 and 246, with 1551 connections.
The multiplex is directed and unweighted, stored as edges list in the file
    
    CKM-Physicians-Innovation_multiplex.edges

with format

    layerID nodeID nodeID weight

(Note: all weights are set to 1)

The IDs of all layers are stored in 

    CKM-Physicians-Innovation_layers.txt

The IDs of nodes can be found in the file

    CKM-Physicians-Innovation_nodes.txt

The values for the 14 columns (attributes) are:

1. node id
2. city
3. adoption date
4. med_sch_yr
5. meetings
6. jours
7. free_time
8. discuss
9. clubs
10. friends
11. community
12. patients
13. proximity
14. specialty

For the values assigned to each attribute see the original paper.


### License

The CKM-PHYSICIANS-INNOVATION MULTIPLEX DATASET is provided "as is" and without warranties as to performance or quality or any other warranties whether expressed or implied. 


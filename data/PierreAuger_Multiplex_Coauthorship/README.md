

# PIERRE AUGER MULTIPLEX

###### Last update: 18 March 2015

### Reference and Acknowledgments

This README file accompanies the dataset representing the multiplex coauthorship network in the internal report repository of the Pierre Auger Collaboration (<http://www.auger.org/>), the largest team of scientists working about ultra-high energy cosmic rays. 
If you use this dataset in your work either for analysis or for visualization, you should acknowledge/cite the following paper:
	
	“Identifying Modular Flows on Multilayer Networks Reveals Highly Overlapping Organization in Interconnected Systems”
	Manlio De Domenico, Andrea Lancichinetti, Alex Arenas, and Martin Rosvall
	Physical Review X 5, 011027 (2015)
	
that can be found at the following URLs:

<http://journals.aps.org/prx/abstract/10.1103/PhysRevX.5.011027>


This work has been supported by European Commission FET-Proactive project PLEXMATH (Grant No. 317614), the European project devoted to the investigation of multi-level complex systems and has been developed at the Alephsys Lab. 

Visit

PLEXMATH: <http://www.plexmath.eu/>

ALEPHSYS: <http://deim.urv.cat/~alephsys/>

for further details.



### Description of the dataset

The multiplex consists of layers corresponding to different working tasks within the Pierre Auger Collaboration. We considered all submissions between 2010 and 2012 and assigned each report to L=16 layers according to its keywords and its content, with manual disambiguation to avoid spurious results from an automated process.
The authors acknowledge all members of the Pierre Auger Collaboration for kindly providing access to the metadata of its repository for internal technical reports and Dr. M. Settimo for kindly helping to classify all reports to the proper task(s).

The multiplex network used in the paper makes use of 16 layers corresponding to:

1. Neutrinos
2. Detector
3. Enhancements
4. Anisotropy
5. Point-source
6. Mass-composition
7. Horizontal
8. Hybrid-reconstruction
9. Spectrum
10. Photons
11. Atmospheric
12. SD-reconstruction
13. Hadronic-interactions
14. Exotics
15. Magnetic
16. Astrophysical-scenarios

There are 514 nodes, labelled with integer ID between 1 and 514, and 7153 coauthorship connections.
The multiplex is undirected (with only one direction specified) and weighted, stored as edges list in the file
    
    pierreauger_multiplex.edges

with format

    layerID nodeID nodeID weight

The IDs of all layers are stored in 

    pierreauger_layers.txt

The IDs of nodes are not provided to preserve the privacy of the original authors.



### License

This PIERRE AUGER MULTIPLEX DATASET is made available under the Open Database License: <http://opendatacommons.org/licenses/odbl/1.0/>. Any rights in individual contents of the database are licensed under the Database Contents License: <http://opendatacommons.org/licenses/dbcl/1.0/>

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
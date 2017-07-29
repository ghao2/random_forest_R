##############################################################
#            Random Forest with Balanced Tree
#
#             Author: Guoxuan Hao
##############################################################
#                        Summary
#
#The idea of this implementation is to retain the background theory
#of random forest, but in the same time to create trees that are
#of more balanced structures.
#
#The purpose of this implementation is to investigate the relationship
#between tree structures and the variance. And hopefully can find a 
#way to reduce variance.
#
##############################################################



##############################################################
#
#This part is Class declaration part
#it contains two classes, Node and BinaryTree
#
##############################################################


#declare class Node

Node<-setRefClass("Node",
                  fields = list(variable_vector = "ANY", #store the randomly selected variables
                                variable_position = "ANY", #which variable is the best to use for this node
                                
                                #the data set only contains variables that are
                                #in variable_vectors
                                
                                data_set ="ANY", #what is the current data set for this node
                                
                                split_value = "ANY", #what is the split point for child nodes
                                class_type = "ANY", #what is the predicted class by this node
                                node_type = "ANY", #1 is leaf nodes, 2 is leaf node with fake children, 3 is fake node, 4 is internal node, 5 is semi-internal,
                                #only left child exists
                                node_number = "ANY",
                                left = "ANY",
                                right = "ANY"),
                  
                  methods = list(
                    
                    getVarVec = function(){
                      return (variable_vector)
                    },
                    
                    getVarPos = function(){
                      return (variable_position)
                    },
                    
                    getDataSet = function(){
                      return (data_set)
                    },
                    
                    getSplitValue = function(){
                      return (split_value)
                    },
                    
                    getClassType = function(){
                      return (class_type)
                    },
                    
                    getNodeType = function(){
                      return (node_type)
                    },
                    
                    getNodeNumber = function(){
                      return (node_number)
                    },
                    
                    getLeft = function(){
                      return (left)
                    },
                    
                    getRight = function(){
                      return (right)
                    },
                    
                    setVarVec = function(x){
                      variable_vector<<-x
                    },
                    
                    setVarPos = function(x){
                      variable_position<<-x
                    },
                    
                    setDataSet = function(x){
                      data_set<<-x
                    },
                    
                    setSplitValue = function(x){
                      split_value<<-x
                    },
                    
                    setNodeType = function(x){
                      node_type<<-x
                    },
                    
                    setNodeNumber = function(x){
                      node_number<<-x
                    },
                    
                    setClassType = function(x){
                      class_type<<-x
                    },
                    
                    setLeft = function(x){
                      left<<-x
                    },
                    
                    setRight = function(x){
                      right<<-x
                    },
                    
                    #determine the variable, position, and class type
                    
                    #NOT OPTIMAL
                    #this section contains findVarPosType(), findSplitForVar() calcMinGini()
                    findVarPos = function(){
                      number_variable<-length(variable_vector)
                      x_value<-seq(1:number_variable)*0
                      gini_index<-seq(1:number_variable)*0
                      
                      #get the split value and smallest Gini Index under each variable
                      for(index in 1:number_variable){
                        info_vector<-findSplitForVar(data_set[,index], data_set[,ncol(data_set)]) #second input value is response
                        x_value[index]<-info_vector[1]
                        gini_index[index]<-info_vector[2]
                      }
                      
                      #find the globally smallest Gini Index and the position of that variable in variable_vector
                      min_a<-gini_index[1]
                      min_position<-1
                      
                      for(val in 1:number_variable){
                        
                        if(gini_index[val]<min_a){
                          min_a<-gini_index[val]
                          min_position<-val
                        }
                      }
                      
                      #determine the best variable and best split
                      best_var_position<-min_position
                      best_split<-x_value[min_position]
                      
                      #return
                      setVarPos(best_var_position)
                      setSplitValue(best_split)
                      
                    },
                    
                    #NOT OPTIMAL
                    #input is a column of the dataframe, in the format of a vector and the label column
                    #return the split value and the Gini Index to this split value
                    findSplitForVar = function (x, y) {
                      
                      #get the number of observations
                      b<-length(x)
                      a<-seq(1:b)*0
                      
                      #calculate Gini Index for each of the splits defined by each observation.
                      for(val in 1:b){
                        a[val]<-calcMinGini(x,y,val)
                      }
                      
                      #find the minimal Gini Index and cooresponding x value position
                      min_a<-a[1]
                      min_position<-1
                      
                      for(val in 1:b){
                        if(a[val]<min_a){
                          min_a<-a[val]
                          min_position<-val
                        }
                      }
                      
                      return_vector<-c(x[min_position], min_a)
                      
                      #return the x value cooresponding to the minimal Gini Index, and the minimal Gini Index value
                      return (return_vector)
                    },
                    
                    #NOT OPTIMAL
                    #calculate Gini Index, given the data and split position
                    calcMinGini = function(x,y,split_pos){
                      
                      #let a represent the group of elements in x, 1 represent left, 0 represent right
                      #calc number of elements in each side
                      b<-length(x)
                      
                      number_left<-0
                      number_yes_l<-0
                      number_right<-0
                      number_yes_r<-0
                      
                      #calc number of above variables
                      for(val in 1:b){
                        if(x[val]<=x[split_pos]){
                          
                          number_left<-number_left+1
                          
                          if(y[val]==1){
                            number_yes_l<-number_yes_l+1
                          }
                        }
                        
                        else{
                          if(y[val]==1){
                            number_yes_r<-number_yes_r+1
                          }
                        }
                      }
                      
                      number_right<-b-number_left
                      
                      #prevent the denominator from being 0
                      if(number_left==0){
                        number_left<-1
                      }
                      
                      if(number_right==0){
                        number_right<-1
                      }
                      
                      #let p represent the probability of class 1
                      #calculate the left group
                      p_l<-number_yes_l/number_left
                      p_r<-number_yes_r/number_right
                      
                      #the formula to calc Gini Index
                      gini_index<-number_left*2*p_l*(1-p_l)+number_right*2*p_r*(1-p_r)
                      
                      return (gini_index)
                      
                    },
                    
                    #split data set into two parts return the left side
                    getSplitDataLeft = function(){
                      
                      the_var<-variable_vector[variable_position]
                      
                      #two functions, first, we need to use substitute(), which substitute the variable that is defined in the argument "list".
                      #substitute() returns a parse tree, which needs to be evaluated by eval() to return an actual object.
                      #the variable can be charactor type or name type
                      data_left<-eval(substitute(data_set[data_set$variable<=split_value,], list(variable = the_var)))
                      
                      return (data_left)
                    },
                    
                    #split data set into two parts return the right side
                    getSplitDataRight = function(){
                      
                      the_var<-variable_vector[variable_position]
                      
                      data_right<-eval(substitute(data_set[data_set$variable>split_value,], list(variable = the_var)))
                      
                      return (data_right)
                    },
                    
                    #                     #check out if this node is a leaf node
                    #                     isLeaf = function(){
                    #                       if(is.null(left) && is_null(right)){
                    #                         is_leaf<-TRUE
                    #                       }
                    #                       else{
                    #                         is_leaf<-FALSE
                    #                       }
                    #                       
                    #                       return (is_leaf)
                    #                     },
                    #                     
                    #NOT OPTIMAL
                    #calc the class type, the class type really depends on the input y
                    findClassType = function(){
                      
                      #remember vector[i] return a scalar, so have to round it up with []
                      y_column<-data_set[,ncol(data_set)]
                      number_row<-nrow(data_set)
                      
                      number_one_y<-0
                      
                      for(val in 1:number_row){
                        
                        if(y_column[val]==1){
                          number_one_y<-number_one_y+1
                        }
                      }
                      
                      p<-number_one_y/number_row
                      
                      #if p is greater than 0.5. then class type is 1
                      if(p>=0.5){
                        setClassType(1)
                      }
                      else{
                        setClassType(0)
                      }
                      
                    },
                    
                    #determine if it is a leaf with fake children, if its p value is either 0 or 1, then it is.
                    #it is checking if all observations belongs to only one class
                    #but let's assume even if they are not all one class, it's still possible to produce empty child
                    checkInOneClass = function(){
                      
                      y_column<-data_set[,ncol(data_set)]
                      number_row<-nrow(data_set)
                      
                      number_one_y<-0
                      
                      for(val in 1:number_row){
                        
                        if(y_column[val]==1){
                          number_one_y<-number_one_y+1
                        }
                      }
                      
                      p<-number_one_y/number_row
                      
                      return(p)
                      
                    },
                    
                    #get number of observations in this data set
                    number_observations = function(){
                      
                      return (nrow(data_set))
                    },
                    
                    #check empty
                    checkEmpty = function(){
                      a<-nrow(data_set)
                      
                      if(a==0){
                        return (TRUE)
                      }
                      else(
                        return (FALSE)
                      )
                    }
                    
                  )
                  
)

#declare class BinaryTree

BinaryTree<-setRefClass("BinaryTree",
                        fields = list(
                          root = "Node",
                          max_cap = "numeric",
                          
                          total_no_node = "ANY",
                          no_unfake_node = "ANY", #node type 1, 2, and 4.
                          no_real_leaf = "ANY" #
                        ),
                        
                        methods = list(
                          
                          getRoot = function(){
                            return (root)
                          },
                          
                          getMaxCap = function(){
                            return (max_cap)
                          },
                          
                          getTotalNoNode = function (){
                            return (total_no_node)
                          },
                          
                          getNoUnfakeNode = function () {
                            return (no_unfake_node)
                          },
                          
                          getNoRealLeaf = function (){
                            return (no_real_leaf)
                          },
                          
                          setTotalNoNode = function(x){
                            total_no_node<<-x
                          },
                          
                          setNoUnfakeNode = function(x){
                            no_unfake_node<<-x
                          },
                          
                          setNoRealLeaf = function(x){
                            no_real_leaf<<-x
                          },
                          
                          setMaxCap = function(x){
                            max_cap<<-x
                          },
                          
                          
                          #set the the node number of root is 1.
                          configRoot = function(x){
                            
                            #set the node type in the node, at begenning it is a leaf node
                            #all these parameters might be set outside the class.
                            root$setNodeNumber(1)
                            root$setNodeType(1)
                            
                            #set the var position
                            root$findVarPos()
                            
                            #set the class type of root
                            root$findClassType()
                            
                            setTotalNoNode(1)
                            
                            setNoUnfakeNode(1)
                            
                            setNoRealLeaf(1)
                          },
                          
                          #1, grow tree until certain conditions are met
                          #2, prune the tree once growing of tree is done.
                          
                          #input is the number of unfake node in the tree, which is the max_cap of the tree
                          growTree = function(){
                            
                            while(getNoRealLeaf()!=0 && getNoUnfakeNode()<max_cap){
                              
                              #check its parent node, if it is a leaf with fake children, if not, split it into two nodes
                              #add left node, then check the max_cap again, if valid, add right, otherwise end loop.
                              
                              #get new node position
                              new_node_pos<-getTotalNoNode()+1
                              
                              #calculate its parent position
                              #if the mode 2 returns 1, it is odd number, otherwise it is even
                              
                              #if it is odd
                              if(new_node_pos%%2){
                                parent_node_pos<-(new_node_pos-1)/2
                              }
                              else{
                                parent_node_pos<-new<-new_node_pos/2
                              }
                              
                              #get its parent node
                              parent_node<-getNode(parent_node_pos)
                              parent_type<-parent_node$getNodeType()
                              
                              #determine the parent node type, only two possiblities at this point, either leaf node or fake node
                              #the leaf node could potentially be a leaf with fake children, but we don't know yet
                              
                              #if it is a fake node, we simply assign two fake children to it
                              if(parent_type==3){
                                
                                left_node_number<-parent_node$getNodeNumber()*2
                                right_node_number<-parent_node$getNodeNumber()*2+1
                                
                                leftFakeNode<-Node$new(variable_vector=NULL, 
                                                       variable_position=NULL, 
                                                       data_set=NULL,
                                                       split_value=NULL,
                                                       class_type=NULL,
                                                       node_type=3,
                                                       node_number=left_node_number,
                                                       left=NULL,
                                                       right=NULL)
                                
                                rightFakeNode<-Node$new(variable_vector=NULL, 
                                                        variable_position=NULL, 
                                                        data_set=NULL,
                                                        split_value=NULL,
                                                        class_type=NULL,
                                                        node_type=3,
                                                        node_number=right_node_number,
                                                        left=NULL,
                                                        right=NULL)
                                
                                
                                parent_node$setLeft(leftFakeNode)
                                
                                parent_node$setRight(rightFakeNode)
                                
                                #update the number of total
                                update_total_no<-getTotalNoNode()+2
                                
                                setTotalNoNode(update_total_no)
                                
                                #update the number of unfake node, no need in this case
                                #update the number of real node, no need in this case
                                
                                
                              }
                              
                              #else, it is a leaf node
                              else{
                                p<-parent_node$checkInOneClass()
                                
                                #check if all-in-one-class, assign fake children to it
                                if(p==1 || p==0){
                                  
                                  left_node_number<-parent_node$getNodeNumber()*2
                                  right_node_number<-parent_node$getNodeNumber()*2+1
                                  
                                  leftFakeNode<-Node$new(variable_vector=NULL, 
                                                         variable_position=NULL, 
                                                         data_set=NULL,
                                                         split_value=NULL,
                                                         class_type=NULL,
                                                         node_type=3,
                                                         node_number=left_node_number,
                                                         left=NULL,
                                                         right=NULL)
                                  
                                  rightFakeNode<-Node$new(variable_vector=NULL, 
                                                          variable_position=NULL, 
                                                          data_set=NULL,
                                                          split_value=NULL,
                                                          class_type=NULL,
                                                          node_type=3,
                                                          node_number=right_node_number,
                                                          left=NULL,
                                                          right=NULL)
                                  
                                  
                                  #set left and right for parent
                                  parent_node$setLeft(leftFakeNode)
                                  
                                  parent_node$setRight(rightFakeNode)
                                  
                                  parent_node$setNodeType(2)
                                  
                                  #update the number of total
                                  update_total_no<-getTotalNoNode()+2
                                  
                                  setTotalNoNode(update_total_no)
                                  
                                  #update the number of unfake node, no need in this case
                                  #update the number of real leaf, meaning without fake children
                                  update_no_real<-getNoRealLeaf()-1
                                  setNoRealLeaf(update_no_real)
                                  
                                }
                                
                                #else if it is not the two types, then grow the parent node
                                #but, if one of the children is empty, then grow with fake children
                                else{
                                  parent_node$findVarPos()
                                  left_data<-parent_node$getSplitDataLeft()
                                  right_data<-parent_node$getSplitDataRight()
                                  var_vector<-parent_node$getVarVec()
                                  
                                  left_node_number<-parent_node$getNodeNumber()*2
                                  right_node_number<-parent_node$getNodeNumber()*2+1
                                  
                                  #set these new nodes as leaf
                                  leftNode<-Node$new(variable_vector=var_vector, 
                                                     variable_position=NULL, 
                                                     data_set=left_data,
                                                     split_value=NULL,
                                                     class_type=NULL,
                                                     node_type=1,
                                                     node_number=left_node_number,
                                                     left=NULL,
                                                     right=NULL)
                                  
                                  rightNode<-Node$new(variable_vector=var_vector, 
                                                      variable_position=NULL, 
                                                      data_set=right_data,
                                                      split_value=NULL,
                                                      class_type=NULL,
                                                      node_type=1,
                                                      node_number=right_node_number,
                                                      left=NULL,
                                                      right=NULL)
   
                                  #when growing, it is possible the children has empty dataset, therefore, findclass() won't work
                                  if(nrow(left_data)==0 || nrow(right_data)==0){
                                    leftNode$setNodeType(3)
                                    rightNode$setNodeType(3)
                                    
                                    parent_node$setLeft(leftNode)
                                    
                                    parent_node$setRight(rightNode)
                                    
                                    parent_node$setNodeType(2)
                                    
                                    #update the number of total
                                    update_total_no<-getTotalNoNode()+2
                                    
                                    setTotalNoNode(update_total_no)
                                    
                                    #update the number of unfake node, no need in this case
                                    #update the number of real leaf, meaning without fake children
                                    update_no_real<-getNoRealLeaf()-1
                                    setNoRealLeaf(update_no_real)
                                    
                                    
                                  }
                                  #if it can be devided into two sets with data, then grow two leaves
                                  else{
                                    #set class type for leaf node
                                    leftNode$findClassType()
                                    rightNode$findClassType()
                                    
                                    #calc the parameters for the children
                                    leftNode$findVarPos()
                                    
                                    #set left first, then check the max_cap
                                    parent_node$setLeft(leftNode)
                                    
                                    #update the number of total
                                    update_total_no<-getTotalNoNode()+1
                                    setTotalNoNode(update_total_no)
                                    
                                    #update the number of unfake node, no need in this case
                                    update_no_unfake<-getNoUnfakeNode()+1
                                    setNoUnfakeNode(update_no_unfake)
                                    
                                    #update the number of real leaf, meaning without fake children, in this case, it doens't increase
#                                     update_no_real<-getNoRealLeaf()+1
#                                     setNoRealLeaf(update_no_real)
                                    
                                    #update parent node type to be 5
                                    parent_node$setNodeType(5)
                                    
                                    #check the max_cap, if less than, then add the right child
                                    if(getNoUnfakeNode()<getMaxCap()){
                                      
                                      #calc the parameters for the children
                                      rightNode$findVarPos()
                                      
                                      parent_node$setRight(rightNode)
                                      
                                      #update the number of total
                                      update_total_no<-getTotalNoNode()+1
                                      setTotalNoNode(update_total_no)
                                      
                                      #update the number of unfake node, no need in this case
                                      update_no_unfake<-getNoUnfakeNode()+1
                                      setNoUnfakeNode(update_no_unfake)
                                      
                                      #update the number of real leaf, meaning without fake children
                                      update_no_real<-getNoRealLeaf()+1
                                      setNoRealLeaf(update_no_real)
                                      
                                      #update parent node type to be 4
                                      parent_node$setNodeType(4)
                                      
                                    }
                                  }
                                  
                                  parent_node$findClassType()
                                  
                                  
                                }
                                
                              }
                              
                              
                            }
                            
                            
                          },
                          
                          #it has two parts, getNode and getNodeRC
                          getNode =function(input_pos){
                            
                            node<-getNodeRC(input_pos, getRoot())
                            
                            return (node)
                            
                          },
                          
                          getNodeRC =function (input_pos, node){
                            
                            
                            if(is.null(node)){
                              return (NULL)
                            }
                            
                            if(input_pos==node$getNodeNumber()){
                              return (node)
                            }
                            else{
                              
                              left_side<-node$getLeft()
                              right_side<-node$getRight()
                              
                              if(is.null(getNodeRC(input_pos,left_side))){
                                return (getNodeRC(input_pos,right_side))
                              }
                              else{
                                return (getNodeRC(input_pos,left_side))
                              }
                              
                            }
                            
                            
                          },
                          
                          #This is not very neccesary, however it can reduce the memory consumption when number of trees are large
                          #get rid of the fake nodes
                          pruneTree = function(){
                            
                          },
                          
                          
                          #calculate the number of nodes in the tree
                          findNodeNumber = function(){
                            
                            node_number<-returnNodeNumber(root)
                            
                            return (node_number)
                          },
                          
                          #to get the number of nodes in a tree
                          returnNodeNumber = function(x){
                            
                            if(is.null(x)){
                              return (0)
                            }
                            
                            else{
                              
                              return (returnNodeNumber(x$getLeft())+returnNodeNumber(x$getRight())+1)
                            }
                          },
                          
                          
                          #classification
                          #given an observation, return the class type of it, x is a list
                          cls_observation = function (x){
                            
                            #tranform x into new x which is with only the features required in this tree
                            
                            #first, get variable vector stored in node
                            variable_vec_node<-root$getVarVec()
                            
                            #setup a value vector to store the values of x
                            length_vec<-length(variable_vec_node)
                            value_vector<-seq(1:length_vec)*0
                            
                            length_list<-length(x)
                            
                            #get the values from the list
                            for(val in 1:length_vec){
                              
                              for(inter_val in 1:length_list){
                                if(variable_vec_node[val]==names(x)[inter_val])
                                  
                                  #select each column of x, because x here is a data frame, each column has only one value
                                  value_vector[val]<-x[,inter_val]
                              }
                            }
                            
                            #classify x according to its corresponding value at each position.
                            node<-getCorrectNode(value_vector, root)
                            
                            class_x<-node$getClassType()
                            
                          },
                          
                          #input_x is a vector, not a data frme, so no names.
                          getCorrectNode = function(input_x, input_node){
                            
                            node_type<-input_node$getNodeType()
                            
                            if(node_type==1 || node_type==2){
                              return (input_node)
                            }
                            
                            var_pos<-input_node$getVarPos()
                            split_value<-input_node$getSplitValue()
                            
                            if(input_x[var_pos]<=split_value){
                              return (getCorrectNode(input_x, input_node$getLeft()))
                            }
                            else{
                              #it is possible the right side of the node is null, if that is the case, return itself.
                              if(is.null(input_node$getRight())){
                                return (input_node)
                              }
                              else{
                                return (getCorrectNode(input_x, input_node$getRight()))
                              }
                              
                            }
                            
                          }
                          
                          
                        )
)


########################################################################
#
#
#this part is using random forest to predict the response for input data
########################################################################
#the function of predicting the response for a single observation
rf_pred_observation<-function(input_x, input_data_set, number_trees, number_ran_var, cap_nodes){
  
  result_vec<-seq(1:number_trees)*0
  
  for(val in 1:number_trees){
    
    name_set<-names(input_data_set)
    
    response<-name_set[8]
    
    inputs<-name_set[1:7]
    
    var_vec<-sample(inputs, number_ran_var)
    
    #notice, has to use drop = FALSE, otherwise, when only selecting one column, it will return a vector not a data frame
    input_data<-input_data_set[,var_vec, drop = FALSE]
    
    response_data<-input_data_set[,response]
    
    tree_data_set<-cbind(input_data,response_data)
    
    #make a boostrap sample
    n_row<-nrow(input_data)
    
    sample_row<-sample(1:n_row, size=n_row, replace = TRUE)
    
    tree_data_set<-tree_data_set[sample_row,]
    
    #check OOB condition
    exists<-0
    
    for(row_index in 1:n_row){
      
      #this is comparing two lists, returned by select a row of a data.frame and let 'drop' equals TRUE
      if(all(tree_data_set[row_index, var_vec, drop=TRUE]%in%input_x[,var_vec, drop = TRUE])){
        exists<-1
      }
    }
    
    if(exists==1)
    {
      result_vec[val]<--1
    }
    else{
    
      new_node<-Node$new(variable_vector=var_vec, 
                         variable_position=NULL, 
                         data_set=tree_data_set,
                         split_value=NULL,
                         class_type=NULL,
                         node_type=3,
                         node_number=NULL,
                         left=NULL,
                         right=NULL)
      
      new_tree<-BinaryTree$new(root=new_node,max_cap=cap_nodes,total_no_node=NULL,no_unfake_node=NULL,no_real_leaf=NULL)
      
      new_tree$configRoot()
      
      new_tree$growTree()
      
      input_for_pred<-input_x[,var_vec, drop = FALSE]
      
      result_vec[val]<-new_tree$cls_observation(input_for_pred)
    }
    
  }
  
  #calculate final prediction
  
  #calc the number of "non -1" elements
  sum_non_n1<-0
  #calc the number of 1 elements
  sum_non_1<-0
  
  for(val in 1:number_trees){
    if(result_vec[val]!=-1){
      sum_non_n1<-sum_non_n1+1
    }
    
    if(result_vec[val]==1){
      sum_non_1<-sum_non_1+1
    }
  }
  
  #calc the probability
  p<-sum_non_1/sum_non_n1
  
  if(is.nan(p)){
    
    a<-sample(c(0,1), size = 1)
    
    return (a)
  }
  
  if(p>=0.5){
    return (1)
  }
  else{
    return (0)
  }
  
}

#############################################
#
#this part is data preparation
#
#data set is called EPLStats.csv
#############################################

data<-read.table("EPLStats.csv", sep = ",", header = TRUE)

pruned_data<-data[,3:10]

n_row<-nrow(pruned_data)

pruned_data[,8]<-as.numeric(pruned_data[,8])

for(val in 1:n_row){
  if(pruned_data[val,8]=="2"){
    pruned_data[val,8]<-1
  }
  else{
    pruned_data[val,8]<-0
  }
}



########################################################################
#
#
#this part is using random forest to predict the response for input data
#modify the parameters at line 948, they are:x-input, training dataset, number of trees, number of random varables, and maximum number of nodes
########################################################################

pred_vec<-seq(1:n_row)*0

for(val in 1:n_row){
  
  #modify the parameters here
  #modify the parameters: they are:x-input, training dataset, number of trees, number of random varables, and maximum number of nodes
  pred_vec[val]<-rf_pred_observation(pruned_data[val,],pruned_data,10,2,5)
  
}

real_vec<-pruned_data[,8]

same_vector<-seq(1:n_row)*0

for(index in 1:n_row){
  if(pred_vec[index]==real_vec[index]){
    same_vector[index]<-1
  }
}

#calc the error rate
error_rate<-1-mean(same_vector)




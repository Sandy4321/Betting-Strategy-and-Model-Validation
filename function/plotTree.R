plotTree <- function(dfm, parallel=FALSE){
  # Plotting Classification Trees with the plot.rpart and rattle pckages
  
  require(rpart, quietly=TRUE)				  # Popular decision tree algorithm
  require(rattle, quietly=TRUE)					# Fancy tree plot
  require(rpart.plot, quietly=TRUE)			# Enhanced tree plots
  require(RColorBrewer, quietly=TRUE)		# Color selection for fancy tree plot
  require(party, quietly=TRUE)					# Alternative decision tree algorithm
  require(partykit, quietly=TRUE)				# Convert rpart object to BinaryTree
  
  # Make big tree
  form <- as.formula(Class ~ .)
  tree.1 <- rpart(form,data=dfm,control=rpart.control(minsplit=20,cp=0))
  # 
  plot(tree.1)					# Will make a mess of the plot
  text(tree.1)
  # 
  prp(tree.1)					# Will plot the tree
  prp(tree.1,varlen=3)				# Shorten variable names

  # Interatively prune the tree
  new.tree.1 <- prp(tree.1,snip=TRUE)$obj # interactively trim the tree
  prp(new.tree.1) # display the new tree
  #
  #-------------------------------------------------------------------
  tree.2 <- rpart(form,dfm)			# A more reasonable tree
  prp(tree.2)                                     # A fast plot													
  fancyRpartPlot(tree.2)				# A fancy plot from rattle
  #
  #-------------------------------------------------------------------
  # Plot a tree built with RevoScaleR
  # Construct a model formula
  sdNames <- names(segmentationData)
  X <- as.vector(sdNames[-c(1,2,3)])
  form <- as.formula(paste("Class","~", paste(X,collapse="+")))
  # Run the model
  rx.tree <- rxDTree(form, data = segmentationData,maxNumBins = 100,
             minBucket = 10,maxDepth = 5,cp = 0.01, xVal = 0)
  # Plot the tree						
  prp(rxAddInheritance(rx.tree))
  fancyRpartPlot(rxAddInheritance(rx.tree))
  
}



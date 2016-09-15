#iris-setosa=>1
#Iris-versicolor=>2
#Iris-virginica=>3
inputdataSet<-read.csv("D:\\SC package\\iris.csv",header=FALSE)
inputdataSet<-as.matrix(inputdataSet)
#inputdataSet
input_matrix<-matrix(,nrow(inputdataSet),ncol(inputdataSet))
mi<-matrix(,1,ncol(inputdataSet)-1)
mil<-matrix(,1,ncol(inputdataSet)-1)
mih<-matrix(,1,ncol(inputdataSet)-1)
Ialpha<-c(0.29,0.15,0.38,0.37)
redan<-c(0.12,0.057,0.152)
upperbound<-matrix(,1,ncol(inputdataSet)-1)
lowerbound<-matrix(,1,ncol(inputdataSet)-1)
I_alpha<-matrix(0,1,ncol(input_matrix)-1)
alpha<-0.5
beta<-0.5
eta<-1.5
objective_fn<-matrix(0,1,4)
temp<-0
Clow<-matrix(0,1,4)
Cmedium<-matrix(0,1,4)
Chigh<-matrix(0,1,4)
sigmalow<-matrix(0,1,4)
sigmahigh<-matrix(0,1,4)
sigmamedium<-matrix(0,1,4)
minrange<-matrix(0,1,10)
maxrange<-matrix(0,1,10)
similarity_matrix<-matrix(1,4,4)


#fuzzification process ie normalising using max-min 
Fuzzification<-function(){
	
	max_values<-apply(inputdataSet,2,max)
	min_values<-apply(inputdataSet,2,min)
	#cat("Max_values:",max_values)
	#cat("Min_values:",min_values)
	for(i in 1:ncol(inputdataSet)-1){
		#cat("min_value [i]:",min_values[i] "/n")
		for(j in 1:nrow(inputdataSet)){
			#print(j,i)
			inputdataSet[j,i]<-(inputdataSet[j,i]-min_values[i])/(max_values[i]-min_values[i])
		}
	}
	#print("inputdata Set after fuzzification")
	#print(inputdataSet)
	return (inputdataSet)
}




#CalcParameters(input_matrix)
#calculation of parameters center and radius
#CalcParameters<-function(input_matrix){
#print(input_matrix)
j<-0
	for(i in 1:ncol(input_matrix)-1){
		mi[i] = mean(input_matrix[,i])
		upperbound[i] = max(input_matrix[,i])
		lowerbound[i]= min(input_matrix[,i])
		
	}	
		print("mi")
		print(mi)
		print(upperbound)
		print(lowerbound)
		#print(input_matrix)
	for(i in 2:ncol(input_matrix)-1){
		#print(input_matrix[,i])
		for(j in 1:nrow(input_matrix)){
			#print(input_matrix[j,i])
			if(input_matrix[j,i]>lowerbound[i]){
				if (input_matrix[j,i]<mi[i]){				
					minrange[j]<-input_matrix[j,i]
					
					
				}
			}
			if(input_matrix[j,i]>mi[i]){
				 if(input_matrix[j,i]<upperbound[i]){
					maxrange[j]<-input_matrix[j,i]
				}
			}
		}
		mil[i]<-mean(minrange,na.rm=TRUE)
		mih[i]<-mean(maxrange,na.rm=TRUE)
	}
	Clow<-mil
	Chigh<-mih
	Cmedium<-mi
	print("Clow")
	print(Clow)
	print("Cmedium")
	print(Cmedium)
	print("Chigh")
	print(Chigh)




#Sigma(radius) computations



	for(i in 2:ncol(input_matrix)-1){
	#print(input_matrix[,i])
		for(j in 1:nrow(input_matrix)){
			#print(Cmedium)
			#print(Clow)
			sigmalow[i]<-2*(Cmedium[i] - Clow[i])
			sigmahigh[i]<-2*(Chigh[i]-Cmedium[i])
			A<-sigmalow[i]*(upperbound[i] - Cmedium[i])
			A<-A+ sigmahigh[i]*(Cmedium[i]-lowerbound[i])
			B<-upperbound[i]-lowerbound[i]
			sigmamedium[i]<-eta*(A/B)
			

		}
	}
	print("sigmalow")
	print(sigmalow)
	print("simgahigh")
	print(sigmahigh)
	print("sigmamedium")
	print(sigmamedium)




#Feature Selection using Relevance and redundancy




t<-0
#CalcIalpha<-function(input_matrix){
	for(i in 2:ncol(input_matrix)-1){
		for(j in 1:nrow(input_matrix)){
			p<-input_matrix[j,i]
			q<-input_matrix[j,5]
			temp <-((min(p,q)/ nrow(input_matrix))*exp(alpha))
			temp <-temp / ((p*q)/nrow(input_matrix)^2) *exp(alpha-1)
			temp <-temp-1 
			t<- t+temp
		}
		I_alpha[i]<-t/(alpha*(alpha-1))		
	}
	
		
#}

Selected_feature<-matrix(0,1,ncol(input_matrix)-1)
#Calculation of Redancy& relan
#Calc_Relevance<-function(Ialpha){
	k<-1
	index<-which.max(Ialpha)
	Selected_feature[1,k]<-index
	k<-k+1
	print("***************Relevance*****************")
	print(Ialpha)
	print("***********Selected feature using Relevance************")
	print(Selected_feature)	




		
#calc redancy
	for(i in 2:ncol(input_matrix)-1){
		if(i!=index){
			for(j in 1:nrow(input_matrix)){
				p<-input_matrix[j,i]
				q<-input_matrix[j,index]
				temp = ((min(p,q)/ nrow(input_matrix))*exp(alpha))
				temp = temp / ((p*q)/nrow(input_matrix)^2) *exp(alpha-1)
				temp = temp-1 
				t<-t+temp
			}
			redann<-t/(alpha*(alpha-1))	
		}
	}



	
	j<-1
	for(i in 2:ncol(input_matrix)-1){
	#print(i)
		if(i != index){
			#print(index)
			#print("in IF")
			
			temp<-sum(redan)
			#print(temp)
			temp<-temp*beta
			#print(temp)
			objective_fn[i]<-Ialpha[i]-temp
			#print(objective_fn[i])		
			Selected_feature[1,k]<-which.max(objective_fn)
			
			#print(Selected_feature)
	

		}
	}



	print("****************Redundancy**************")
	print(redan)
	print("*************Objective function**************")
	print(objective_fn)

	print("**********************Selected feature*****************")
	print(Selected_feature)
	#return(Selected_feassssture)

			
#}

temp<-0
temp2<-0
temp3<-0
for(i in 1:nrow(input_matrix)){
	temp<-temp+(input_matrix[i,1]-input_matrix[i,2]^2)
	temp2<-temp2+(input_matrix[i,1]-input_matrix[i,3]^2)
	temp3<-temp3+(input_matrix[i,1]-input_matrix[i,4]^2)
}
dist<-sqrt(temp)
similarity_matrix[1,2]<-dist
similarity_matrix[2,1]<-dist
dist<-sqrt(temp2)
similarity_matrix[1,3]<-dist
similarity_matrix[3,1]<-dist
dist<-sqrt(temp3)
similarity_matrix[1,4]<-dist
similarity_matrix[4,1]<-dist

for(i in 1:nrow(input_matrix)){
	temp<-temp+(input_matrix[i,2]-input_matrix[i,3]^2)
	temp2<-temp2+(input_matrix[i,2]-input_matrix[i,4]^2)
}
dist<-sqrt(temp)
similarity_matrix[2,3]<-dist
similarity_matrix[3,2]<-dist
dist<-sqrt(temp2)
similarity_matrix[2,4]<-dist
similarity_matrix[4,2]<-dist

for(i in 1:nrow(input_matrix)){
	temp<-temp+(input_matrix[i,3]-input_matrix[i,4]^2)
}
dist<-sqrt(temp)
similarity_matrix[3,4]<-dist
similarity_matrix[4,3]<-dist


input_matrix<-Fuzzification()
#CalcParameters(input_matrix)
#CalcIalpha(input_matrix)
#Calc_Relevance(Ialpha)



c(1:nrow(input_matrix))->distance
c(1:nrow(input_matrix))->class
c(rep(0,3))->countClass

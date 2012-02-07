#' Estimating population size via sample coverage
#' @param z the vector of ．capture histories・ or ．ascertainment records・.
#' @return estimates based on any pair of samples

estN.pair3 <-
		function(z){
	z001=z[1];	z010=z[2];	z011=z[3];	z100=z[4];	z101=z[5];	z110=z[6];	z111=z[7]
	n1=z100+z101+z110+z111
	n2=z010+z011+z110+z111
	n3=z001+z011+z101+z111
	M1=z100+z101+z110+z111+z010+z011
	M2=z100+z101+z110+z111+z001+z011
	M3=z001+z101+z110+z111+z010+z011
	
	#pair(1,2)
	pet1=(n1*n2)/(z110+z111)
	chp1=((n1+1)*(n2+1)/(z110+z111+1)-1)
	chse1=((n1+1)*(n2+1)*(z100+z101)*(z010+z011)/((z111+z110+1)^2*(z111+z110+2)))^.5
	ch1=exp(1.96*(log(1+chse1^2/(chp1-M1)^2))^(0.5))
	ch1l=M1+(chp1-M1)/ch1
	ch1u=M1+(chp1-M1)*ch1
	pa12=cbind(pet1,chp1,chse1,ch1l,ch1u)
	
	#pair(1,3)
	pet2=(n1*n3)/(z101+z111)
	chp2=((n1+1)*(n3+1)/(z101+z111+1)-1)
	chse2=((n1+1)*(n3+1)*(z100+z110)*(z001+z011)/((z111+z101+1)^2*(z111+z101+2)))^.5
	ch2=exp(1.96*(log(1+chse2^2/(chp2-M2)^2))^(0.5))
	ch2l=M2+(chp2-M2)/ch2
	ch2u=M2+(chp2-M2)*ch2
	pa13=cbind(pet2,chp2,chse2,ch2l,ch2u)
	
	#pair(2,3)
	pet3=(n3*n2)/(z011+z111)
	chp3=((n3+1)*(n2+1)/(z011+z111+1)-1)
	chse3=((n3+1)*(n2+1)*(z010+z110)*(z001+z101)/((z111+z011+1)^2*(z111+z011+2)))^.5
	ch3=exp(1.96*(log(1+chse3^2/(chp3-M3)^2))^(0.5))
	ch3l=M3+(chp3-M3)/ch3
	ch3u=M3+(chp3-M3)*ch3
	pa23=cbind(pet3,chp3,chse3,ch3l,ch3u)
	
	pair=rbind(pa12,pa13,pa23)
	colnames(pair)=c("Petersen","Chapman","se","cil","ciu")
	rownames(pair)=c("pa12","pa13","pa23")
	pair
}


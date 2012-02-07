#' Number of identified cases in each records
#' @param z the vector of capture histories or ascertainment records.
#' @return Number of identified cases.
#' @author Tsung-Chen Hsieh
#' @examples data(HAV)
#' estN.n(HAV)
#' @export
estN.n=function(z){
z=as.vector(unlist(z))
	estN.n3 <-
			function(z){
		z001=z[1];	z010=z[2];	z011=z[3];	z100=z[4];	z101=z[5];	z110=z[6];	z111=z[7]
		n1=z100+z101+z110+z111
		n2=z010+z011+z110+z111
		n3=z001+z011+z101+z111
		size=cbind(n1,n2,n3)
		size
	}
	
	estN.n4 <-function(z){
		z0001=z[1];	z0010=z[2]; z0011=z[3]; z0100=z[4]; z0101=z[5];
		z0110=z[6]; z0111=z[7]; z1000=z[8]; z1001=z[9]; z1010=z[10];
		z1011=z[11]; z1100=z[12]; z1101=z[13]; z1110=z[14]; z1111=z[15]
		
		n1=z1000+z1001+z1010+z1100+z1011+z1110+z1101+z1111
		n2=z0100+z0101+z0110+z0111+z1100+z1110+z1101+z1111
		n3=z0010+z0011+z0110+z0111+z1010+z1011+z1110+z1111
		n4=z0001+z0011+z0101+z0111+z1001+z1011+z1101+z1111
		size=cbind(n1,n2,n3,n4)
		size
	}
	estN.n5 <-function(z){
		z00001=z[1]; z00010=z[2]; z00011=z[3]; z00100=z[4];
		z00101=z[5]; z00110=z[6]; z00111=z[7]; z01000=z[8]; 
		z01001=z[9]; z01010=z[10]; z01011=z[11]; z01100=z[12];
		z01101=z[13]; z01110=z[14]; z01111=z[15]; z10000=z[16]; 
		z10001=z[17]; z10010=z[18]; z10011=z[19]; z10100=z[20];
		z10101=z[21]; z10110=z[22]; z10111=z[23]; z11000=z[24];
		z11001=z[25]; z11010=z[26]; z11011=z[27]; z11100=z[28];
		z11101=z[29]; z11110=z[30]; z11111=z[31]
		
		n1=z10000+z10001+z10010+z10011+z10100+z10101+z10110+z10111+z11000+z11001+z11010+z11011+z11100+z11101+z11110+z11111
		n2=z01000+z01001+z01010+z01011+z01100+z01101+z01110+z01111+z11000+z11001+z11010+z11011+z11100+z11101+z11110+z11111
		n3=z00100+z00101+z00110+z00111+z01100+z01101+z01110+z01111+z10100+z10101+z10110+z10111+z11100+z11101+z11110+z11111
		n4=z00010+z00011+z00110+z00111+z01010+z01011+z01110+z01111+z10010+z10011+z10110+z10111+z11010+z11011+z11110+z11111
		n5=z00001+z00011+z00101+z00111+z01001+z01011+z01101+z01111+z10001+z10011+z10101+z10111+z11001+z11011+z11101+z11111
		
		size=cbind(n1,n2,n3,n4,n5)
		size
	}
	
	
	if(length(z) == 7)
		est = estN.n3(z)
	else if(length(z) == 15)
		est = estN.n4(z)		
	else if(length(z) == 31)
		est = estN.n5(z)
	else "Invalid argument error! Please input valid ascertainment records."
	return(est)
}

#' Transform observed data to ascertainment records.
#' @param x 
#' @return ascertainment records
#' @author T.C. Hsieh
#' @examples
#' x=matrix(sample(0:1,300,T),ncol=3)
#' as.record(x)
#' @export
as.record=function(x){
	if((sum(x==1)+sum(x==0)) != ncol(x)*nrow(x)) "Invalid argument error! Please input valid ascertainment records."

	else if(ncol(x)==3){
		lab=c("000","001","010","011","100","101","110","111")
		y=apply(x,1,function(x)paste(x[1],x[2],x[3],sep=""))
		table(factor(y,levels=lab))[-1]
	}
	else if(ncol(x)==4){
		lab=c("0000","0001","0010","0011","0100","0101","0110","0111","1000","1001","1010","1011","1100","1101","1110","1111")
		y=apply(x,1,function(x)paste(x[1],x[2],x[3],x[4],sep=""))
		table(factor(y,levels=lab))[-1]
	}
	else if(ncol(x)==5){
		lab=c("00000","00001","00010","00011","00100","00101","00110","00111","01000","01001","01010","01011",
				"01100","01101","01110","01111","10000","10001","10010","10011","10100","10101","10110",
				"10111","11000","11001","11010","11011","11100","11101","11110","11111")
		y=apply(x,1,function(x)paste(x[1],x[2],x[3],x[4],x[5],sep=""))
		table(factor(y,levels=lab))[-1]
	}
	else "Invalid argument error! Please input valid ascertainment records."
}

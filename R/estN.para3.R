estN.para3 <-function(z,nhat){
	z001=z[1];	z010=z[2];	z011=z[3];	z100=z[4];	z101=z[5];	z110=z[6];	z111=z[7]
	M=z001+z010+z011+z100+z101+z110+z111
	if(nhat<=M){
		para=cbind(u1=0,u2=0,u3=0,r12=0,r13=0,r23=0,r123=0)
		colnames(para)=c("u1","u2","u3","r12","r13","r23","r123")
		para
	}
	else{
		n1=z100+z101+z110+z111
		n2=z010+z011+z110+z111
		n3=z001+z011+z101+z111
		
		u1=n1/nhat
		u2=n2/nhat
		u3=n3/nhat
		
		r12=nhat*(z110+z111)/(n1*n2)-1
		r13=nhat*(z101+z111)/(n1*n3)-1
		r23=nhat*(z011+z111)/(n2*n3)-1
		r123=((nhat^2)*z111)/(n1*n2*n3)-r12-r13-r23-1
		rN=u1*u2*(r12*(r13+r23)-r123)+u1*u3*(r13*(r12+r23)-r123)+u2*u3*(r23*(r12+r13)-r123)
		para=cbind(u1,u2,u3,r12,r13,r23,r123)
		colnames(para)=c("u1","u2","u3","r12","r13","r23","r123")
		para
	}
}


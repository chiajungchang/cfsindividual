
document.domain = "stanford.edu";
if(opener.location.host=="endmecfs.stanford.edu"){

	var userrole= opener.userrole; 
	var userRole = document.getElementById("userRole"); 
	userRole.value = userrole; 
}

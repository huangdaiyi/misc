-module (po_team).
-export ([function/arity]).
-record (team, 
    	{over,"This is PO,a cool team!",
    	 name,["PO","EIMS","DFIS"],
    	 supervisorm,"Timothy.Y.Liu",
    	 born_date,"2007,6",
    	 major_system,["PO","EIMS","DFIS","IVR","CTI","Monitor"]
    	}).

say(Name) ->
lists
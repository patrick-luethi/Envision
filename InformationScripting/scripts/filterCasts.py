# filterCasts

classUses = Query.executeQuery('ast -t=CastExpression|attribute -at=castType -input|uses -input -t=Class', [])

def hasTypeIdMethod( cl ):
    for method in cl.methods:
        if method.name == "typeIdStatic":
            return True
    return False

for tuple in classUses[0].tuples("uses"):
    if hasTypeIdMethod(tuple.used):
        values = [("ast", tuple.user)]
        Query.result.add(Tuple(values))

Query.result = Query.toParent(["-t=CastExpression", "-addAs=node"], [Query.result])[0]

void generateNodeMap() {
  Map docTypes = ["document" : 0, "prov:Plan" : 1, "ddmore:Model" : 2, "ddmore:Dataset" : 3, "ddmore:Output" : 4]
  
  int id = 0
  entityRows.each { k, v ->
      Map node = [:]
      
      id = Integer.parseInt(v.id)-1 // Subtract 1 from everything as the spreadsheet is 1 based by d3 is zero based
      node["name"] = v.name
      String type = Entity.getDocType( v.name )
      node["group"] = docTypes.keySet().contains(type) ? docTypes[type] : 0
      node["id"] = id
      
      d3["nodes"] << node
      
      v["derivedFrom"].each{
        d3["links"] << link(Integer.parseInt(it)-1, id, 0)
      }
      v["generatedBy"].each{
        d3["links"] << link(Integer.parseInt(it)-1, id, 1)
      }
  }
  
  activityRows.each { k, v ->
      Map node = [:]
      
      // ID of this node is the id of the last entity, plus the id of the activity
      int activityId = id+Integer.parseInt(v.id)
      node["name"] = v.process
      node["group"] = 5 // Activity = group 5
      node["id"] = activityId
      
      // Create a node for the activity
      d3["nodes"] << node
      
      v.inputEntities.each {
        int inputId = Integer.parseInt(it)-1
        d3["links"] << link(inputId, activityId, 2) // Input link
      }
      v.outputEntities.each {
        int outputId = Integer.parseInt(it)-1
        d3["links"] << link(activityId, outputId,  3) // Output Link
      }
      if(v.dependencyActivityId) {
        try { int inDependency = Integer.parseInt(v.dependencyActivityId) + id
        d3["links"] << link(inDependency, activityId, 4) } // Dependency
        catch(Exception e) {}
      } else if(v.relatedActivityId) {
        int inDependency = Integer.parseInt(v.relatedActivityId) + id
        d3["links"] << link(inDependency, activityId, 4) // Dependency
      }
  }
  
  // Create the file
  File document = nextFile("d3.json")
  
  // Export to JSON
  JsonBuilder builder = new JsonBuilder()
  
  builder d3
  
  String json = builder.toPrettyString()
  
  // Write out the JSON
  document.write(json)
  
  
}

public static String getDocType(String location) {
  String docType = "document"
  
  if(location.endsWith(".r")) {
    docType = "prov:Plan"
  } else if (location.endsWith(".mdl")) {
    docType ="ddmore:Model"
  } else if(location.endsWith(".csv")) {
    docType ="ddmore:Dataset"
  } else if(location.endsWith(".lst")) {
    docType ="ddmore:Output"
  }
  docType
}     

Map link(int source, int target, int value ) {
  ["source" : source, "target" : target, "value" : value]
}
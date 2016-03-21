namespace TempoTango

module Graph =

  type shape = DoubleCircle | Circle
  type graph_kind = DiGraph | Graph

  type node = {
    name  : string;
    shape : shape;
    start : bool;
  }

  type edge = {
    label : string;
    s     : node;
    t     : node;
  }

  type graph = {
    kind     : graph_kind;
    title    : string;
    settings : string list;
    nodes    : node list;
    edges    : edge list;
  }

  let NewGraph title =
    { kind = DiGraph; title = title; settings = []; nodes = []; edges = [] }

  let FindNode {nodes = nodes} name =
    List.tryFind (fun n -> n.name = name) nodes

  let AddNodeInternal graph node =
    match FindNode graph node.name with
      | None -> { graph with nodes = node :: graph.nodes }
      | Some(_) -> graph

  let AddFinal graph name =
    AddNodeInternal graph { name = name; shape = DoubleCircle; start = false }

  let AddStart graph name =
    AddNodeInternal graph { name = name; shape = Circle; start = true }

  let AddStartFinal graph name =
    AddNodeInternal graph { name = name; shape = DoubleCircle; start = true }

  let AddNode graph name =
    AddNodeInternal graph { name = name; shape = Circle; start = false }

  let edge graph s_name t_name label =
    let s = FindNode graph s_name;
    let t = FindNode graph t_name;
    { graph with edges = { label = label; s = s.Value; t = t.Value } :: graph.edges}

  open Printf

  let GraphKindtoString = function
    | DiGraph -> "digraph"
    | Graph   -> "graph"

  let PrintNodes out nodes =
    let print_node_list nodes =
      if not (List.isEmpty nodes) then
        (List.iter (fun n -> fprintf out "\"%s\" " n.name) nodes;
         fprintf out ";\n")
    let (circle, double) = List.partition (fun n -> n.shape = Circle) nodes
    fprintf out "\tnode [shape = doublecircle]; ";
    print_node_list double;
    fprintf out "\tnode [shape = circle]; ";
    print_node_list circle;
    let starts = List.filter (fun n -> n.start) nodes
    List.iter (fun n -> fprintf out "\t\"_nil_%s\" [style=\"invis\",fixedsize=true];\n\t\"_nil_%s\" -> \"%s\";\n" n.name n.name n.name) starts

  let PrintEdges out =
    List.iter (fun e ->
      fprintf out "\t\"%s\" -> \"%s\" [ label = \"%s\" ];\n" e.s.name e.t.name e.label
    )

  let PrintGraph out { kind = kind; title = title; settings = settings; nodes = nodes; edges = edges } =
    fprintf out "%s %s {\n" (GraphKindtoString kind) title;
    List.iter (fprintf out "\t%s\n") settings;
    PrintNodes out nodes;
    PrintEdges out edges;
    fprintf out "}"

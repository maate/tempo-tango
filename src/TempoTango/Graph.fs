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

  let new_graph title =
    { kind = DiGraph; title = title; settings = []; nodes = []; edges = [] }

  let find_node {nodes = nodes} name =
    List.find (fun n -> n.name = name) nodes

  let AddNode_internal graph node =
    try
    begin
      ignore (find_node graph node.name);
      graph
    end with
      | failwith -> { graph with nodes = node :: graph.nodes }

  let add_final graph name =
    AddNode_internal graph { name = name; shape = DoubleCircle; start = false }

  let AddStart graph name =
    AddNode_internal graph { name = name; shape = Circle; start = true }

  let add_start_final graph name =
    AddNode_internal graph { name = name; shape = DoubleCircle; start = true }

  let AddNode graph name =
    AddNode_internal graph { name = name; shape = Circle; start = false }

  let edge graph s_name t_name label =
    { graph with edges = { label = label; s = find_node graph s_name; t = find_node graph t_name } :: graph.edges}

  open Printf

  let graph_kind_to_string = function
    | DiGraph -> "digraph"
    | Graph   -> "graph"

  let print_nodes out nodes =
    let print_node_list nodes =
      if not (List.isEmpty nodes) then
        (List.iter (fun n -> fprintf out "\"%s\" " n.name) nodes;
         fprintf out ";\n")
    in
    let (circle, double) = List.partition (fun n -> n.shape = Circle) nodes in
    fprintf out "\tnode [shape = doublecircle]; ";
    print_node_list double;
    fprintf out "\tnode [shape = circle]; ";
    print_node_list circle;
    let starts = List.filter (fun n -> n.start) nodes in // used by be find_all instead of filter
    List.iter (fun n -> fprintf out "\t\"_nil_%s\" [style=\"invis\"];\n\t\"_nil_%s\" -> \"%s\";\n" n.name n.name n.name) starts

  let print_edges out =
    List.iter (fun e ->
      fprintf out "\t\"%s\" -> \"%s\" [ label = \"%s\" ];\n" e.s.name e.t.name e.label
    )

  let print_graph out { kind = kind; title = title; settings = settings; nodes = nodes; edges = edges } =
    fprintf out "%s %s {\n" (graph_kind_to_string kind) title;
    List.iter (fprintf out "\t%s\n") settings;
    print_nodes out nodes;
    print_edges out edges;
    fprintf out "}\n"

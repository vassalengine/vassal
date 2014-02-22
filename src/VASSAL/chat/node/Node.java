/*
 * $Id$
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
/*
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: May 29, 2003
 */
package VASSAL.chat.node;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.logging.Logger;

import VASSAL.tools.PropertiesEncoder;
import VASSAL.tools.SequenceEncoder;

/**
 * Base class for the hierarchical server model.
 * Each node has a name, a list of children, and arbitrary extra information
 * encoded in the {@link #getInfo} string. Each node can be identified
 * globally by a path name. Messages sent to a node generally broadcast to
 * all descendents of the node.
 */
public class Node implements MsgSender {
  private static Logger logger = Logger.getLogger(MsgSender.class.getName());
  private String id;
  private String info;
  private Node parent;
  private List<Node> children = new ArrayList<Node>();

  public Node(Node parent, String id, String info) {
    this.parent = parent;
    this.id = id;
    this.info = info;
  }

  public String getId() {
    return id;
  }

  public String getInfo() {
    return info;
  }

  public String getInfoProperty(String propName) {
    try {
      return new PropertiesEncoder(info).getProperties().getProperty(propName);
    }
    catch (IOException e) {
      return null;
    }
  }

  public void setInfo(String info) {
    this.info = info;
  }

  public void setParent(Node parent) {
    this.parent = parent;
  }

  public Node getParent() {
    return parent;
  }

  public void remove(Node child) {
    logger.finer("Removing "+child+" from "+this); //$NON-NLS-1$ //$NON-NLS-2$
    children.remove(child);
  }

  public void add(Node child) {
    // FIXME: added this to find out what is calling add(null)
    if (child == null) {
      throw new NullPointerException("child == null");
    }

    if (child.parent != null) {
      child.parent.remove(child);
    }
    logger.finer("Adding "+child+" to "+this); //$NON-NLS-1$ //$NON-NLS-2$
    children.add(child);
    child.setParent(this);
  }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof Node)) return false;

    final Node node = (Node) o;

    if (id != null ? !id.equals(node.id) : node.id != null) return false;

    return true;
  }

  public int hashCode() {
    return (id != null ? id.hashCode() : 0);
  }

  public String toString() {
    return super.toString()+"[id="+id+"]"; //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * return the child node with the given id, or null if no match
   * @param id
   * @return
   */
  public Node getChild(String id) {
    for (Node n : getChildren()) {
      if (id.equals(n.getId())) {
        return n;
      }
    }
    return null;
  }

  /**
   * Return the descendant node with the given path relative to this node
   * @param path
   * @return
   */
  public Node getDescendant(String path) {
    Node n = this;
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(path,'/');
    while (st.hasMoreTokens() && n != null) {
      String id = st.nextToken();
      n = n.getChild(id);
    }
    return n;
  }

  public void send(String msg) {
    for (Node n : getChildren()) {
      n.send(msg);
    }
  }

  public Node[] getLeafDescendants() {
    ArrayList<Node> l = new ArrayList<Node>();
    addLeaves(this, l);
    return l.toArray(new Node[l.size()]);
  }

  private void addLeaves(Node base, List<Node> l) {
    if (base.isLeaf()) {
      l.add(base);
    }
    else {
      for (Node n : base.getChildren()) {
        addLeaves(n,l);
      }
    }
  }

  public boolean isLeaf() {
    return false;
  }

  public Node[] getChildren() {
    synchronized (children) {
      return children.toArray(new Node[children.size()]);
    }
  }

  /**
   * Constructs from a path name.
   * Instantiates parent nodes with appropriate names as necessary.
   * @param base the top-level ancestor of the node to be built.
   *             Its name is not included in the path name
   * @param path
   * @return
   */
  public static Node build(Node base, String path) {
    Node node = null;
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(path, '/');
    while (st.hasMoreTokens()) {
      String childId = st.nextToken();
      node = base.getChild(childId);
      if (node == null) {
        node = new Node(base, childId, null);
        base.add(node);
      }
      base = node;
    }
    return node;
  }

  /**
   * Builds a Node from a pathAndInfo string
   * @see #getPathAndInfo
   * @param base
   * @param pathAndInfo
   * @return
   */
  public Node buildWithInfo(Node base, String pathAndInfo) {
    Node node = null;
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(pathAndInfo, '/');
    while (st.hasMoreTokens()) {
      SequenceEncoder.Decoder st2 = new SequenceEncoder.Decoder(st.nextToken(),'=');
      String childId = st2.nextToken();
      String childInfo = st2.nextToken();
      node = base.getChild(childId);
      if (node == null) {
        node = new Node(base, childId, null);
        base.add(node);
      }
      node.setInfo(childInfo);
      base = node;
    }
    return node;
  }

  private List<Node> getPathList() {
    ArrayList<Node> path = new ArrayList<Node>();
    for (Node n = this; n != null && n.getId() != null; n = n.getParent()) {
      path.add(n);
    }
    return path;
  }

  public String getPath() {
    synchronized (children) {
      SequenceEncoder se = new SequenceEncoder('/');
      List<Node> path = getPathList();
      for (ListIterator<Node> i = path.listIterator(path.size());
           i.hasPrevious(); ) {
        se.append(i.previous().getId());
      }
      return se.getValue();
    }
  }

  /**
   * Return a string in the format parentId=parentInfo/childId=childInfo/...
   * @return
   */
  public String getPathAndInfo() {
    synchronized (children) {
      SequenceEncoder se = new SequenceEncoder('/');
      List<Node> path = getPathList();
      for (ListIterator<Node> i = path.listIterator(path.size()-1);
           i.hasPrevious(); ) {
        Node n = i.previous();
        SequenceEncoder se2 =
          new SequenceEncoder(n.getId(),'=').append(n.getInfo());
        se.append(se2.getValue());
      }
      return se.getValue();
    }
  }
}

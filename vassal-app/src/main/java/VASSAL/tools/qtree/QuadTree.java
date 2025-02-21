/*
 * $Id: QuadTree.java 9188 2015-04-17 04:37:20Z swampwallaby $
 *
 * Copyright (c) 2015 by Brent Easton
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
 * The MIT License

Copyright (c) 2014 Varun Pant

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal 
in the Software without restriction, including without limitation the rights 
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies 
of the Software, and to permit persons to whom the Software is furnished to do 
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all 
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A 
PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION 
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

 */

package VASSAL.tools.qtree;

import java.util.ArrayList;
import java.util.List;

/**
 * Datastructure: A point Quad Tree for representing 2D data. Each
 * region has the same ratio as the bounds for the tree.
 * <p/>
 * The implementation currently requires pre-determined bounds for data as it
 * can not rebalance itself to that degree.
 */
public class QuadTree<T> implements Cloneable {


  private QNode<T> root_;
  private int count_ = 0;

  /**
   * Constructs a new quad tree.
   *
   * @param {double} minX Minimum x-value that can be held in tree.
   * @param {double} minY Minimum y-value that can be held in tree.
   * @param {double} maxX Maximum x-value that can be held in tree.
   * @param {double} maxY Maximum y-value that can be held in tree.
   */
  public QuadTree(double minX, double minY, double maxX, double maxY) {
    this.root_ = new QNode<>(minX, minY, maxX - minX, maxY - minY, null);
  }

  public QuadTree() {

  }
  /**
   * Returns a reference to the tree's root node.  Callers shouldn't modify nodes,
   * directly.  This is a convenience for visualization and debugging purposes.
   *
   * @return {Node} The root node.
   */
  public QNode<T> getRootNode() {
    return this.root_;
  }

  public void setRootNode(QNode<T> root) {
    this.root_ = root;
  }

  /**
   * Sets the value of an (x, y) point within the quad-tree.
   *
   * @param {double} x The x-coordinate.
   * @param {double} y The y-coordinate.
   * @param {T} value The value associated with the point.
   */
  public void set(double x, double y, T value) {

    final QNode<T> root = this.root_;
    if (x < root.getX() || y < root.getY() || x > root.getX() + root.getW() || y > root.getY() + root.getH()) {
      throw new QuadTreeException("Out of bounds : (" + x + ", " + y + ")");
    }
    if (this.insert(root, new QPoint<>(x, y, value))) {
      this.count_++;
    }
  }

  /**
   * Gets the value of the point at (x, y) or null if the point is empty.
   *
   * @param {double} x The x-coordinate.
   * @param {double} y The y-coordinate.
   * @param {T} opt_default The default value to return if the node doesn't
   *                 exist.
   * @return {*} The value of the node, the default value if the node
   *         doesn't exist, or undefined if the node doesn't exist and no default
   *         has been provided.
   */
  public T get(double x, double y, T opt_default) {
    final QNode<T> node = this.find(this.root_, x, y);
    return node != null ? node.getPoint().getValue() : opt_default;
  }

  /**
   * Removes a point from (x, y) if it exists.
   *
   * @param {double} x The x-coordinate.
   * @param {double} y The y-coordinate.
   * @return {T} The value of the node that was removed, or null if the
   *         node doesn't exist.
   */
  public T remove(double x, double y) {
    final QNode<T> node = this.find(this.root_, x, y);
    if (node != null) {
      final T value = node.getPoint().getValue();
      node.setPoint(null);
      node.setNodeType(QNodeType.EMPTY);
      this.balance(node);
      this.count_--;
      return value;
    }
    else {
      return null;
    }
  }

  /**
   * Returns true if the point at (x, y) exists in the tree.
   *
   * @param {double} x The x-coordinate.
   * @param {double} y The y-coordinate.
   * @return {boolean} Whether the tree contains a point at (x, y).
   */
  public boolean contains(double x, double y) {
    return this.get(x, y, null) != null;
  }

  /**
   * @return {boolean} Whether the tree is empty.
   */
  public boolean isEmpty() {
    return this.root_.getNodeType() == QNodeType.EMPTY;
  }

  /**
   * @return {number} The number of items in the tree.
   */
  public int getCount() {
    return this.count_;
  }

  /**
   * Removes all items from the tree.
   */
  public void clear() {
    this.root_.setNw(null);
    this.root_.setNe(null);
    this.root_.setSw(null);
    this.root_.setSe(null);
    this.root_.setNodeType(QNodeType.EMPTY);
    this.root_.setPoint(null);
    this.count_ = 0;
  }

  /**
   * Returns an array containing the coordinates of each point stored in the tree.
   * @return {Array.<Point>} Array of coordinates.
   */
  public QPoint<T>[] getKeys() {
    final List<QPoint<T>> arr = new ArrayList<>();
    this.traverse(this.root_, new QFunc<>() {
      @Override
      public void call(QuadTree<T> quadTree, QNode<T> node) {
        arr.add(node.getPoint());
      }
    });
    return arr.toArray((QPoint<T>[]) new QPoint[0]);
  }

  /**
   * Returns a list containing all values stored within the tree.
   * @return {List<T>} The values stored within the tree.
   */
  public List<T> getValues() {
    final List<T> arr = new ArrayList<>();
    this.traverse(this.root_, new QFunc<>() {
      @Override
      public void call(QuadTree<T> quadTree, QNode<T> node) {
        arr.add(node.getPoint().getValue());
      }
    });

    return arr;
  }

  public QPoint<T>[] searchIntersect(final double xmin, final double ymin, final double xmax, final double ymax) {
    final List<QPoint<T>> arr = new ArrayList<>();
    this.navigate(this.root_, new QFunc<>() {
      @Override
      public void call(QuadTree<T> quadTree, QNode<T> node) {
        final QPoint<T> pt = node.getPoint();
        if (!(pt.getX() < xmin || pt.getX() > xmax || pt.getY() < ymin || pt.getY() > ymax)) {
          arr.add(node.getPoint());
        }

      }
    }, xmin, ymin, xmax, ymax);
    return arr.toArray((QPoint<T>[]) new QPoint[0]);
  }

  public QPoint<T>[] searchWithin(final double xmin, final double ymin, final double xmax, final double ymax) {
    final List<QPoint<T>> arr = new ArrayList<>();
    this.navigate(this.root_, new QFunc<>() {
      @Override
      public void call(QuadTree<T> quadTree, QNode<T> node) {
        final QPoint<T> pt = node.getPoint();
        if (pt.getX() > xmin && pt.getX() < xmax && pt.getY() > ymin && pt.getY() < ymax) {
          arr.add(node.getPoint());
        }
      }
    }, xmin, ymin, xmax, ymax);
    return arr.toArray((QPoint<T>[]) new QPoint[0]);
  }

  public void navigate(QNode<T> node, QFunc<T> func, double xmin, double ymin, double xmax, double ymax) {
    switch (node.getNodeType()) {
    case LEAF:
      func.call(this, node);
      break;

    case POINTER:
      if (intersects(xmin, ymax, xmax, ymin, node.getNe()))
        this.navigate(node.getNe(), func, xmin, ymin, xmax, ymax);
      if (intersects(xmin, ymax, xmax, ymin, node.getSe()))
        this.navigate(node.getSe(), func, xmin, ymin, xmax, ymax);
      if (intersects(xmin, ymax, xmax, ymin, node.getSw()))
        this.navigate(node.getSw(), func, xmin, ymin, xmax, ymax);
      if (intersects(xmin, ymax, xmax, ymin, node.getNw()))
        this.navigate(node.getNw(), func, xmin, ymin, xmax, ymax);
      break;
    }
  }

  private boolean intersects(double left, double bottom, double right, double top, QNode<T> node) {
    return !(node.getX() > right ||
      (node.getX() + node.getW()) < left ||
      node.getY() > bottom ||
      (node.getY() + node.getH()) < top);
  }
  /**
   * Clones the quad-tree and returns the new instance.
   * @return {QuadTree} A clone of the tree.
   */
  @Override
  public QuadTree<T> clone() {
    final double x1 = this.root_.getX();
    final double y1 = this.root_.getY();
    final double x2 = x1 + this.root_.getW();
    final double y2 = y1 + this.root_.getH();
    final QuadTree<T> clone = new QuadTree<>(x1, y1, x2, y2);
    // This is inefficient as the clone needs to recalculate the structure of the
    // tree, even though we know it already.  But this is easier and can be
    // optimized when/if needed.
    this.traverse(this.root_, new QFunc<>() {
      @Override
      public void call(QuadTree<T> quadTree, QNode<T> node) {
        clone.set(node.getPoint().getX(), node.getPoint().getY(), node.getPoint().getValue());
      }
    });


    return clone;
  }

  /**
   * Traverses the tree depth-first, with quadrants being traversed in clockwise
   * order (NE, SE, SW, NW).  The provided function will be called for each
   * leaf node that is encountered.
   * @param {QuadTree.Node} node The current node.
   * @param {function(QuadTree.Node)} fn The function to call
   *     for each leaf node. This function takes the node as an argument, and its
   *     return value is irrelevant.
   * @private
   */
  public void traverse(QNode<T> node, QFunc<T> func) {
    switch (node.getNodeType()) {
    case LEAF:
      func.call(this, node);
      break;

    case POINTER:
      this.traverse(node.getNe(), func);
      this.traverse(node.getSe(), func);
      this.traverse(node.getSw(), func);
      this.traverse(node.getNw(), func);
      break;
    }
  }

  /**
   * Finds a leaf node with the same (x, y) coordinates as the target point, or
   * null if no point exists.
   * @param {QuadTree.Node} node The node to search in.
   * @param {number} x The x-coordinate of the point to search for.
   * @param {number} y The y-coordinate of the point to search for.
   * @return {QuadTree.Node} The leaf node that matches the target,
   *     or null if it doesn't exist.
   * @private
   */
  public QNode<T> find(QNode<T> node, double x, double y) {
    QNode<T> resposne = null;
    switch (node.getNodeType()) {
    case EMPTY:
      break;

    case LEAF:
      resposne = node.getPoint().getX() == x && node.getPoint().getY() == y ? node : null;
      break;

    case POINTER:
      resposne = this.find(this.getQuadrantForPoint(node, x, y), x, y);
      break;

    default:
      throw new QuadTreeException("Invalid nodeType");
    }
    return resposne;
  }

  /**
   * Inserts a point into the tree, updating the tree's structure if necessary.
   * @param {.QuadTree.Node} parent The parent to insert the point
   *     into.
   * @param {QuadTree.Point} point The point to insert.
   * @return {boolean} True if a new node was added to the tree; False if a node
   *     already existed with the correpsonding coordinates and had its value
   *     reset.
   * @private
   */
  private boolean insert(QNode<T> parent, QPoint<T> point) {
    Boolean result = false;
    switch (parent.getNodeType()) {
    case EMPTY:
      this.setPointForNode(parent, point);
      result = true;
      break;
    case LEAF:
      if (parent.getPoint().getX() == point.getX() && parent.getPoint().getY() == point.getY()) {
        this.setPointForNode(parent, point);
        result = false;
      }
      else {
        this.split(parent);
        result = this.insert(parent, point);
      }
      break;
    case POINTER:
      result = this.insert(
        this.getQuadrantForPoint(parent, point.getX(), point.getY()), point);
      break;

    default:
      throw new QuadTreeException("Invalid nodeType in parent");
    }
    return result;
  }

  /**
   * Converts a leaf node to a pointer node and reinserts the node's point into
   * the correct child.
   * @param {QuadTree.Node} node The node to split.
   * @private
   */
  private void split(QNode<T> node) {
    final QPoint<T> oldPoint = node.getPoint();
    node.setPoint(null);

    node.setNodeType(QNodeType.POINTER);

    final double x = node.getX();
    final double y = node.getY();
    final double hw = node.getW() / 2;
    final double hh = node.getH() / 2;

    node.setNw(new QNode<>(x, y, hw, hh, node));
    node.setNe(new QNode<>(x + hw, y, hw, hh, node));
    node.setSw(new QNode<>(x, y + hh, hw, hh, node));
    node.setSe(new QNode<>(x + hw, y + hh, hw, hh, node));

    this.insert(node, oldPoint);
  }

  /**
   * Attempts to balance a node. A node will need balancing if all its children
   * are empty or it contains just one leaf.
   * @param {QuadTree.Node} node The node to balance.
   * @private
   */
  private void balance(QNode<T> node) {
    switch (node.getNodeType()) {
    case EMPTY:
    case LEAF:
      if (node.getParent() != null) {
        this.balance(node.getParent());
      }
      break;

    case POINTER: {
      final QNode<T> nw = node.getNw();
      final QNode<T> ne = node.getNe();
      final QNode<T> sw = node.getSw();
      final QNode<T> se = node.getSe();
      QNode<T> firstLeaf = null;

      // Look for the first non-empty child, if there is more than one then we
      // break as this node can't be balanced.
      if (nw.getNodeType() != QNodeType.EMPTY) {
        firstLeaf = nw;
      }
      if (ne.getNodeType() != QNodeType.EMPTY) {
        if (firstLeaf != null) {
          break;
        }
        firstLeaf = ne;
      }
      if (sw.getNodeType() != QNodeType.EMPTY) {
        if (firstLeaf != null) {
          break;
        }
        firstLeaf = sw;
      }
      if (se.getNodeType() != QNodeType.EMPTY) {
        if (firstLeaf != null) {
          break;
        }
        firstLeaf = se;
      }

      if (firstLeaf == null) {
        // All child nodes are empty: so make this node empty.
        node.setNodeType(QNodeType.EMPTY);
        node.setNw(null);
        node.setNe(null);
        node.setSw(null);
        node.setSe(null);

      }
      else if (firstLeaf.getNodeType() == QNodeType.POINTER) {
        // Only child was a pointer, therefore we can't rebalance.
        break;

      }
      else {
        // Only child was a leaf: so update node's point and make it a leaf.
        node.setNodeType(QNodeType.LEAF);
        node.setNw(null);
        node.setNe(null);
        node.setSw(null);
        node.setSe(null);
        node.setPoint(firstLeaf.getPoint());
      }

      // Try and balance the parent as well.
      if (node.getParent() != null) {
        this.balance(node.getParent());
      }
    }
    break;
    }
  }

  /**
   * Returns the child quadrant within a node that contains the given (x, y)
   * coordinate.
   * @param {QuadTree.Node} parent The node.
   * @param {number} x The x-coordinate to look for.
   * @param {number} y The y-coordinate to look for.
   * @return {QuadTree.Node} The child quadrant that contains the
   *     point.
   * @private
   */
  private QNode<T> getQuadrantForPoint(QNode<T> parent, double x, double y) {
    final double mx = parent.getX() + parent.getW() / 2;
    final double my = parent.getY() + parent.getH() / 2;
    if (x < mx) {
      return y < my ? parent.getNw() : parent.getSw();
    }
    else {
      return y < my ? parent.getNe() : parent.getSe();
    }
  }

  /**
   * Sets the point for a node, as long as the node is a leaf or empty.
   * @param {QuadTree.Node} node The node to set the point for.
   * @param {QuadTree.Point} point The point to set.
   * @private
   */
  private void setPointForNode(QNode<T> node, QPoint<T> point) {
    if (node.getNodeType() == QNodeType.POINTER) {
      throw new QuadTreeException("Can not set point for node of type POINTER");
    }
    node.setNodeType(QNodeType.LEAF);
    node.setPoint(point);
  }
}

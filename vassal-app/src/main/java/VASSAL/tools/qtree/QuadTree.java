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
public class QuadTree implements Cloneable {


  private QNode root_;
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
    this.root_ = new QNode(minX, minY, maxX - minX, maxY - minY, null);
  }

  public QuadTree() {

  }
  /**
   * Returns a reference to the tree's root node.  Callers shouldn't modify nodes,
   * directly.  This is a convenience for visualization and debugging purposes.
   *
   * @return {Node} The root node.
   */
  public QNode getRootNode() {
    return this.root_;
  }

  public void setRootNode(QNode root) {
    this.root_ = root;
  }

    /**
   * Sets the value of an (x, y) point within the quad-tree.
   *
   * @param {double} x The x-coordinate.
   * @param {double} y The y-coordinate.
   * @param {Object} value The value associated with the point.
   */
  public void set(double x, double y, Object value) {

    final QNode root = this.root_;
    if (x < root.getX() || y < root.getY() || x > root.getX() + root.getW() || y > root.getY() + root.getH()) {
      throw new QuadTreeException("Out of bounds : (" + x + ", " + y + ")");
    }
    if (this.insert(root, new QPoint(x, y, value))) {
      this.count_++;
    }
  }

  /**
   * Gets the value of the point at (x, y) or null if the point is empty.
   *
   * @param {double} x The x-coordinate.
   * @param {double} y The y-coordinate.
   * @param {Object} opt_default The default value to return if the node doesn't
   *                 exist.
   * @return {*} The value of the node, the default value if the node
   * doesn't exist, or undefined if the node doesn't exist and no default
   * has been provided.
   */
  public Object get(double x, double y, Object opt_default) {
    final QNode node = this.find(this.root_, x, y);
    return node != null ? node.getPoint().getValue() : opt_default;
  }

  /**
   * Removes a point from (x, y) if it exists.
   *
   * @param {double} x The x-coordinate.
   * @param {double} y The y-coordinate.
   * @return {Object} The value of the node that was removed, or null if the
   * node doesn't exist.
   */
  public Object remove(double x, double y) {
    final QNode node = this.find(this.root_, x, y);
    if (node != null) {
      final Object value = node.getPoint().getValue();
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
   *
   * @return {Array.<Point>} Array of coordinates.
   */
  public QPoint[] getKeys() {
    final List<QPoint> arr = new ArrayList<>();
    this.traverse(this.root_, new QFunc() {
      @Override
      public void call(QuadTree quadTree, QNode node) {
        arr.add(node.getPoint());
      }
    });
    return arr.toArray(new QPoint[0]);
  }

  /**
   * Returns an array containing all values stored within the tree.
   *
   * @return {Array.<Object>} The values stored within the tree.
   */
  public Object[] getValues() {
    final List<Object> arr = new ArrayList<>();
    this.traverse(this.root_, new QFunc() {
      @Override
      public void call(QuadTree quadTree, QNode node) {
        arr.add(node.getPoint().getValue());
      }
    });

    return arr.toArray(new Object[0]);
  }

  public QPoint[] searchIntersect(final double xmin, final double ymin, final double xmax, final double ymax) {
    final List<QPoint> arr = new ArrayList<>();
    this.navigate(this.root_, new QFunc() {
      @Override
      public void call(QuadTree quadTree, QNode node) {
        final QPoint pt = node.getPoint();
        if (pt.getX() < xmin || pt.getX() > xmax || pt.getY() < ymin || pt.getY() > ymax) {
          return; // Definitely not within the polygon!
        }
        else {
          arr.add(node.getPoint());
        }

      }
    }, xmin, ymin, xmax, ymax);
    return arr.toArray(new QPoint[0]);
  }

  public QPoint[] searchWithin(final double xmin, final double ymin, final double xmax, final double ymax) {
    final List<QPoint> arr = new ArrayList<>();
    this.navigate(this.root_, new QFunc() {
      @Override
      public void call(QuadTree quadTree, QNode node) {
        final QPoint pt = node.getPoint();
        if (pt.getX() > xmin && pt.getX() < xmax && pt.getY() > ymin && pt.getY() < ymax) {
          arr.add(node.getPoint());
        }
      }
    }, xmin, ymin, xmax, ymax);
    return arr.toArray(new QPoint[0]);
  }

  public void navigate(QNode node, QFunc QFunc, double xmin, double ymin, double xmax, double ymax) {
    switch (node.getNodeType()) {
    case LEAF:
      QFunc.call(this, node);
      break;

    case POINTER:
      if (intersects(xmin, ymax, xmax, ymin, node.getNe()))
        this.navigate(node.getNe(), QFunc, xmin, ymin, xmax, ymax);
      if (intersects(xmin, ymax, xmax, ymin, node.getSe()))
        this.navigate(node.getSe(), QFunc, xmin, ymin, xmax, ymax);
      if (intersects(xmin, ymax, xmax, ymin, node.getSw()))
        this.navigate(node.getSw(), QFunc, xmin, ymin, xmax, ymax);
      if (intersects(xmin, ymax, xmax, ymin, node.getNw()))
        this.navigate(node.getNw(), QFunc, xmin, ymin, xmax, ymax);
      break;

    case EMPTY:
      break;
    }
  }

  private boolean intersects(double left, double bottom, double right, double top, QNode node) {
    return !(node.getX() > right ||
      (node.getX() + node.getW()) < left ||
      node.getY() > bottom ||
      (node.getY() + node.getH()) < top);
  }

  /**
   * Clones the quad-tree and returns the new instance.
   *
   * @return {QuadTree} A clone of the tree.
   */
  @Override
  public QuadTree clone() {
    final double x1 = this.root_.getX();
    final double y1 = this.root_.getY();
    final double x2 = x1 + this.root_.getW();
    final double y2 = y1 + this.root_.getH();
    final QuadTree clone = new QuadTree(x1, y1, x2, y2);
    // This is inefficient as the clone needs to recalculate the structure of the
    // tree, even though we know it already.  But this is easier and can be
    // optimized when/if needed.
    this.traverse(this.root_, new QFunc() {
      @Override
      public void call(QuadTree quadTree, QNode node) {
        clone.set(node.getPoint().getX(), node.getPoint().getY(), node.getPoint().getValue());
      }
    });


    return clone;
  }

  /**
   * Traverses the tree depth-first, with quadrants being traversed in clockwise
   * order (NE, SE, SW, NW).  The provided QFunction will be called for each
   * leaf QNode that is encountered.
   *
   * @param {QuadTree.QNode}            QNode The current QNode.
   * @param {QFunction(QuadTree.QNode)} fn The QFunction to call
   *                                    for each leaf QNode. This QFunction takes the QNode as an argument, and its
   *                                    return value is irrelevant.
   * @private
   */
  public void traverse(QNode QNode, QFunc QFunc) {
    switch (QNode.getNodeType()) {
    case LEAF:
      QFunc.call(this, QNode);
      break;

    case POINTER:
      this.traverse(QNode.getNe(), QFunc);
      this.traverse(QNode.getSe(), QFunc);
      this.traverse(QNode.getSw(), QFunc);
      this.traverse(QNode.getNw(), QFunc);
      break;
    case EMPTY:
      break;
    }
  }

  /**
   * Finds a leaf QNode with the same (x, y) coordinates as the target QPoint, or
   * null if no QPoint exists.
   *
   * @param {QuadTree.QNode} QNode The QNode to search in.
   * @param {number}         x The x-coordinate of the QPoint to search for.
   * @param {number}         y The y-coordinate of the QPoint to search for.
   * @return {QuadTree.QNode} The leaf QNode that matches the target,
   * or null if it doesn't exist.
   * @private
   */
  public QNode find(QNode QNode, double x, double y) {
    QNode resposne = null;
    switch (QNode.getNodeType()) {
    case EMPTY:
      break;

    case LEAF:
      resposne = QNode.getPoint().getX() == x && QNode.getPoint().getY() == y ? QNode : null;
      break;

    case POINTER:
      resposne = this.find(this.getuadrantForQPoint(QNode, x, y), x, y);
      break;

    default:
      throw new QuadTreeException("Invalid QNodeType");
    }
    return resposne;
  }

  /**
   * Inserts a QPoint into the tree, updating the tree's structure if necessary.
   *
   * @param {.QuadTree.QNode} parent The parent to insert the QPoint
   *                          into.
   * @param {QuadTree.QPoint} QPoint The QPoint to insert.
   * @return {boolean} True if a new QNode was added to the tree; False if a QNode
   * already existed with the correpsonding coordinates and had its value
   * reset.
   * @private
   */
  private boolean insert(QNode parent, QPoint QPoint) {
    Boolean result = false;
    switch (parent.getNodeType()) {
    case EMPTY:
      this.setPointForQNode(parent, QPoint);
      result = true;
      break;
    case LEAF:
      if (parent.getPoint().getX() == QPoint.getX() && parent.getPoint().getY() == QPoint.getY()) {
        this.setPointForQNode(parent, QPoint);
        result = false;
      }
      else {
        this.split(parent);
        result = this.insert(parent, QPoint);
      }
      break;
    case POINTER:
      result = this.insert(
        this.getuadrantForQPoint(parent, QPoint.getX(), QPoint.getY()), QPoint);
      break;

    default:
      throw new QuadTreeException("Invalid QNodeType in parent");
    }
    return result;
  }

  /**
   * Converts a leaf QNode to a QPointer QNode and reinserts the QNode's QPoint into
   * the correct child.
   *
   * @param {QuadTree.QNode} QNode The QNode to split.
   * @private
   */
  private void split(QNode QNode) {
    final QPoint oldQPoint = QNode.getPoint();
    QNode.setPoint(null);

    QNode.setNodeType(QNodeType.POINTER);

    final double x = QNode.getX();
    final double y = QNode.getY();
    final double hw = QNode.getW() / 2;
    final double hh = QNode.getH() / 2;

    QNode.setNw(new QNode(x, y, hw, hh, QNode));
    QNode.setNe(new QNode(x + hw, y, hw, hh, QNode));
    QNode.setSw(new QNode(x, y + hh, hw, hh, QNode));
    QNode.setSe(new QNode(x + hw, y + hh, hw, hh, QNode));

    this.insert(QNode, oldQPoint);
  }

  /**
   * Attempts to balance a QNode. A QNode will need balancing if all its children
   * are empty or it contains just one leaf.
   *
   * @param {QuadTree.QNode} QNode The QNode to balance.
   * @private
   */
  private void balance(QNode QNode) {
    switch (QNode.getNodeType()) {
    case EMPTY:
    case LEAF:
      if (QNode.getParent() != null) {
        this.balance(QNode.getParent());
      }
      break;

    case POINTER: {
      final QNode nw = QNode.getNw();
      final QNode ne = QNode.getNe();
      final QNode sw = QNode.getSw();
      final QNode se = QNode.getSe();
      QNode firstLeaf = null;

      // Look for the first non-empty child, if there is more than one then we
      // break as this QNode can't be balanced.
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
        // All child QNodes are empty: so make this QNode empty.
        QNode.setNodeType(QNodeType.EMPTY);
        QNode.setNw(null);
        QNode.setNe(null);
        QNode.setSw(null);
        QNode.setSe(null);
      }
      else if (firstLeaf.getNodeType() == QNodeType.POINTER) {
        // Only child was a QPointer, therefore we can't rebalance.
        break;

      }
      else {
        // Only child was a leaf: so update QNode's QPoint and make it a leaf.
        QNode.setNodeType(QNodeType.LEAF);
        QNode.setNw(null);
        QNode.setNe(null);
        QNode.setSw(null);
        QNode.setSe(null);
        QNode.setPoint(firstLeaf.getPoint());
      }

      // Try and balance the parent as well.
      if (QNode.getParent() != null) {
        this.balance(QNode.getParent());
      }
    }
    break;
    }
  }

  /**
   * Returns the child quadrant within a QNode that contains the given (x, y)
   * coordinate.
   *
   * @param {QuadTree.QNode} parent The QNode.
   * @param {number}         x The x-coordinate to look for.
   * @param {number}         y The y-coordinate to look for.
   * @return {QuadTree.QNode} The child quadrant that contains the
   * QPoint.
   * @private
   */
  private QNode getuadrantForQPoint(QNode parent, double x, double y) {
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
   * Sets the QPoint for a QNode, as long as the QNode is a leaf or empty.
   *
   * @param {QuadTree.QNode}  QNode The QNode to set the QPoint for.
   * @param {QuadTree.QPoint} QPoint The QPoint to set.
   * @private
   */
  private void setPointForQNode(QNode QNode, QPoint QPoint) {
    if (QNode.getNodeType() == QNodeType.POINTER) {
      throw new QuadTreeException("Can not set QPoint for QNode of type QPointER");
    }
    QNode.setNodeType(QNodeType.LEAF);
    QNode.setPoint(QPoint);
  }
}


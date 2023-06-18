/*
 * $Id: Qnode.java 9188 2015-04-17 04:37:20Z swampwallaby $
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

public class QNode<T> {

  private double x;
  private double y;
  private double w;
  private double h;
  private QNode<T> opt_parent;
  private QPoint<T> point;
  private QNodeType nodetype = QNodeType.EMPTY;
  private QNode<T> nw;
  private QNode<T> ne;
  private QNode<T> sw;
  private QNode<T> se;

  /**
   * Constructs a new quad tree node.
   *
   * @param {double} x X-coordiate of node.
   * @param {double} y Y-coordinate of node.
   * @param {double} w Width of node.
   * @param {double} h Height of node.
   * @param {Node}   opt_parent Optional parent node.
   * @constructor
   */
  public QNode(double x, double y, double w, double h, QNode<T> opt_parent) {
    this.x = x;
    this.y = y;
    this.w = w;
    this.h = h;
    this.opt_parent = opt_parent;
  }

  public double getX() {
    return x;
  }

  public void setX(double x) {
    this.x = x;
  }

  public double getY() {
    return y;
  }

  public void setY(double y) {
    this.y = y;
  }

  public double getW() {
    return w;
  }

  public void setW(double w) {
    this.w = w;
  }

  public double getH() {
    return h;
  }

  public void setH(double h) {
    this.h = h;
  }

  public QNode<T> getParent() {
    return opt_parent;
  }

  public void setParent(QNode<T> opt_parent) {
    this.opt_parent = opt_parent;
  }

  public void setPoint(QPoint<T> point) {
    this.point = point;
  }

  public QPoint<T> getPoint() {
    return this.point;
  }

  public void setNodeType(QNodeType nodetype) {
    this.nodetype = nodetype;
  }

  public QNodeType getNodeType() {
    return this.nodetype;
  }


  public void setNw(QNode<T> nw) {
    this.nw = nw;
  }

  public void setNe(QNode<T> ne) {
    this.ne = ne;
  }

  public void setSw(QNode<T> sw) {
    this.sw = sw;
  }

  public void setSe(QNode<T> se) {
    this.se = se;
  }

  public QNode<T> getNe() {
    return ne;
  }

  public QNode<T> getNw() {
    return nw;
  }

  public QNode<T> getSw() {
    return sw;
  }

  public QNode<T> getSe() {
    return se;
  }
}
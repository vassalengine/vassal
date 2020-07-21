/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Aug 31, 2002
 * Time: 10:20:59 AM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.tools;

import java.util.Vector;

/**
 * Quicksort implementation so we can sort using JRE 1.1
 * @deprecated Use {@link java.util.Collections.sort} instead.
 */
@Deprecated
public class Sort {
  private static void swap(Vector<Object> v, int i, int j) {
    Object tmp = v.elementAt(i);
    v.setElementAt(v.elementAt(j), i);
    v.setElementAt(tmp, j);
  }

  //------------------------------------------------------------------
  /*
   * quicksort a vector of objects.
   *
   * @param v - a vector of objects
   * @param left - the start index - from where to begin sorting
   * @param right - the last index.
   */
  private static void quicksort(
    Vector<Object> v, int left, int right, Comparator comp) {

    int i, last;

    if (left >= right) { // do nothing if array size < 2
      return;
    }
    swap(v, left, (left + right) / 2);
    last = left;
    for (i = left + 1; i <= right; i++) {
      Object o1 = v.elementAt(i);
      Object o2 = v.elementAt(left);
      if (comp.compare(o1, o2) < 0) {
        swap(v, ++last, i);
      }
    }
    swap(v, left, last);
    quicksort(v, left, last - 1, comp);
    quicksort(v, last + 1, right, comp);
  }
    //------------------------------------------------------------------
    /*
    * quicksort an array of objects.
    *
    * @param arr[] - an array of objects
    * @param left - the start index - from where to begin sorting
    * @param right - the last index.
    private static void quicksort(
       IComparable arr[], int left, int right, boolean ascending) {

        int i, last;

        if (left >= right) { // do nothing if array size < 2
            return;
        }
        swap(arr, left, (left+right) / 2);
        last = left;
        for (i = left+1; i <= right; i++) {
            if (ascending && arr[i].compareTo(arr[left]) < 0 ) {
                swap(arr, ++last, i);
            }
            else if (!ascending && arr[i].compareTo(arr[left]) < 0 ) {
                swap(arr, ++last, i);
            }
        }
        swap(arr, left, last);
        quicksort(arr, left, last-1,ascending);
        quicksort(arr, last+1, right,ascending);
    }
*/
  //------------------------------------------------------------------
  /**
   * Quicksort will rearrange elements when they are all equal. Make sure
   * at least two elements differ
   public static boolean needsSorting(Vector v) {
   IComparable prev = null;
   IComparable curr;
   for (Enumeration e = v.elements(); e.hasMoreElements(); )
   {
   curr = (IComparable)e.nextElement();
   if (prev != null && prev.compareTo(curr) != 0)
   return true;

   prev = curr;
   }
   return false;
   }
   */
  /*
   * Preform a sort using the specified comparitor object.
   */
  public static void quicksort(Vector<Object> v, Comparator comp) {
    quicksort(v, 0, v.size() - 1, comp);
  }

  /**
   * @deprecated Use {@link java.util.Comparator} instead.
   */
  @Deprecated
  public static interface Comparator {
    public int compare(Object o1, Object o2);
  }

  /**
   * Compares two String objects
   * @deprecated Use the natural ordering on Strings instead.
   * @see java.lang.String.compareTo(String)
   */
  @Deprecated
  public static class Alpha implements Comparator {
    @Override
    public int compare(Object o1, Object o2) {
      String s1 = (String) o1;
      String s2 = (String) o2;
      int len = Math.min(s1.length(), s2.length());
      int result = 0;
      for (int i = 0; i < len; ++i) {
        result = s1.charAt(i) - s2.charAt(i);
        if (result != 0) {
          return result;
        }
      }
      if (s1.length() > len) {
        return 1;
      }
      else if (s2.length() > len) {
        return -1;
      }
      else {
        return 0;
      }
    }
  }

}

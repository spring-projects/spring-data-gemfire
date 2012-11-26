/*
 * Copyright 2002-2012 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.util;

/**
 * @author David Turanski
 *
 */
 
 
public abstract class ArrayUtils {

  /**
   * Insert an element into an array.  The element is inserted at the
   * given position, all elements afterwards are moved to the right.
   *
   * @param originalArray array to insert into
   * @param pos position at which to insert the element
   * @param element element to add
   * @return the new array
   */
  public static Object[] insert(Object[] originalArray, int pos, Object element) {
    Object[] newArray = (Object[]) java.lang.reflect.Array.newInstance(
      originalArray.getClass().getComponentType(), originalArray.length + 1);

    
    // copy everything before the given position
    if (pos > 0) {
      System.arraycopy(originalArray, 0, newArray, 0, pos); // does not copy originalArray[pos], where we insert
    }

    // insert
    newArray[pos] = element;

    // copy remaining elements
    if (pos < originalArray.length) {
      System.arraycopy(originalArray, pos, // originalArray[pos] first element copied
          newArray, pos + 1, // newArray[pos + 1] first destination
          originalArray.length - pos); // number of elements left
    }

   
    return newArray;
  }

  /**
   * Remove element from an array.  The element is removed at the
   * specified position, and all remaining elements are moved to the left.
   *
   * @param originalArray array to remove from
   * @param pos position to remove
   * @return the new array
   */
  public static Object[] remove(Object[] originalArray, int pos) {
    Object[] newArray = (Object[])java.lang.reflect.Array.newInstance(
      originalArray.getClass().getComponentType(), originalArray.length - 1);

    

    // Copy everything before
    if (pos > 0) {
      System.arraycopy(originalArray, 0, newArray, 0, pos); // originalArray[pos - 1] is last element copied
    }

  
    // Copy everything after
    if (pos < originalArray.length - 1) {
      System.arraycopy(originalArray, pos + 1, // originalArray[pos + 1] is first element copied
          newArray, pos, // first position to copy into
          originalArray.length - 1 - pos);
    }

   
    return newArray;
  }

  
}

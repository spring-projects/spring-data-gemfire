/*
 * Copyright 2002-2018 the original author or authors.
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
package org.springframework.data.gemfire.function;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.apache.geode.cache.execute.ResultSender;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * Sends collection results using a {@link ResultSender} in chunks determined by batchSize
 *
 * @author David Turanski
 * @since 1.3.0
 */
class BatchingResultSender  {
	private final int batchSize;
	private ResultSender<Object> resultSender;

	public BatchingResultSender(int batchSize, ResultSender<Object> resultSender) {
		Assert.notNull(resultSender, "resultSender cannot be null");
		Assert.isTrue(batchSize >= 0, "batchSize must be >= 0");
		this.batchSize = batchSize;
		this.resultSender = resultSender;
	}


	public void sendResults(Iterable<?> result) {
		if (batchSize == 0) {
			resultSender.lastResult(result);
			return;
		}

		List<Object> chunk = new ArrayList<Object>(batchSize);

		for (Iterator<?> it = result.iterator(); it.hasNext();) {
		    if (chunk.size() < batchSize) {
				chunk.add(it.next());
		    }

			if (chunk.size() == batchSize || !it.hasNext()) {
                if (it.hasNext()) {
                		resultSender.sendResult(chunk);
                } else {
                		resultSender.lastResult(chunk);
                }
           		chunk.clear();
			}
		}
	}


	public void sendArrayResults(Object result) {

		if (batchSize == 0) {
			resultSender.lastResult(result);
			return;
		}

		Assert.isTrue(ObjectUtils.isArray(result));

		int length = Array.getLength(result);

		for (int from =0; from <  length; from += batchSize) {
			int to = Math.min(length,from + batchSize);
			Object chunk = copyOfRange(result,from, to);

			if (to == length -1) {
				resultSender.lastResult(chunk);
			} else {
				resultSender.sendResult(chunk);
			}
		}
	}


	/**
	 * @param result
	 * @param from
	 * @param to
	 * @return
	 */
	private Object copyOfRange(Object result, int from, int to) {

		Class<?> arrayClass = result.getClass();
		int size = to - from;

		if (int[].class.isAssignableFrom(arrayClass)) {
			int[] array = new int[size];
			for(int i = 0; i < size ; ++i){
                array[i] = Array.getInt(result, from + i);
            }
			return array;
		}

		if (float[].class.isAssignableFrom(arrayClass)) {
			float[] array = new float[size];
			for(int i = 0; i < size ; ++i){
                array[i] = Array.getFloat(result, from + i);
            }
			return array;
		}

		if (double[].class.isAssignableFrom(arrayClass)) {
			double[] array = new double[size];
			for(int i = 0; i < size ; ++i){
                array[i] = Array.getDouble(result, from + i);
            }
			return array;
		}

		if (boolean[].class.isAssignableFrom(arrayClass)) {
			boolean[] array = new boolean[size];
			for(int i = 0; i < size ; ++i){
                array[i] = Array.getBoolean(result, from + i);
            }
			return array;
		}

		if (byte[].class.isAssignableFrom(arrayClass)) {
			byte[] array = new byte[size];
			for(int i = 0; i < size ; ++i){
                array[i] = Array.getByte(result, from + i);
            }
			return array;
		}

		if (short[].class.isAssignableFrom(arrayClass)) {
			short[] array = new short[size];
			for(int i = 0; i < size ; ++i){
                array[i] = Array.getShort(result, from + i);
            }
			return array;
		}

		if (long[].class.isAssignableFrom(arrayClass)) {
			long[] array = new long[size];
			for(int i = 0; i < size ; ++i){
                array[i] = Array.getLong(result, from + i);
            }
			return array;
		}

		if (char[].class.isAssignableFrom(arrayClass)) {
			char[] array = new char[size];
			for(int i = 0; i < size ; ++i){
                array[i] = Array.getChar(result, from + i);
            }
			return array;
		}

	    return Arrays.copyOfRange((Object[])result, from, to);

	}
}

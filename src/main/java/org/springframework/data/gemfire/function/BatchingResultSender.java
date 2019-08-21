/*
 * Copyright 2002-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.apache.geode.cache.execute.Function;
import org.apache.geode.cache.execute.ResultSender;

import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * Sends {@link Collection} {@link Function} results using a {@link ResultSender} in chunks
 * determined by {@code batchSize}.
 *
 * @author David Turanski
 * @author Udo Kohlmeyer
 * @author John Blum
 * @since 1.3.0
 */
class BatchingResultSender {

	private final int batchSize;

	private ResultSender<Object> resultSender;

	/**
	 * Constructs a new instance of {@link BatchingResultSender} initialized with the given {@link Integer batch size}
	 * and {@link ResultSender} object used to delegate all send operations.
	 *
	 * @param batchSize {@link Integer} specifying the configured batch size.
	 * @param resultSender {@link ResultSender} used to delegate all send operations.
	 * @throws IllegalArgumentException if {@link ResultSender} is {@literal null}
	 * or {@code batchSize} is less than {@literal 0}.
	 * @see org.apache.geode.cache.execute.ResultSender
	 */
	public BatchingResultSender(int batchSize, ResultSender<Object> resultSender) {

		Assert.notNull(resultSender, "ResultSender must not be null");
		Assert.isTrue(batchSize >= 0, "batchSize must be greater than equal to 0");

		this.batchSize = batchSize;
		this.resultSender = resultSender;
	}

	/**
	 * Returns the configured {@link Integer batchSize} of this batching {@link ResultSender}.
	 *
	 * @return an {@link Integer} value specifying the configured {@link Integer batchSize}
	 * of this batching {@link ResultSender}.
	 */
	public int getBatchSize() {
		return this.batchSize;
	}

	/**
	 * Returns a reference to the configured {@link ResultSender} used to send {@link Function} results.
	 *
	 * @return a reference to the configured {@link ResultSender} used to send {@link Function} results.
	 * @see org.apache.geode.cache.execute.ResultSender
	 */
	public ResultSender<Object> getResultSender() {
		return this.resultSender;
	}

	protected boolean isBatchingDisabled() {
		return !isBatchingEnabled();
	}

	protected boolean isBatchingEnabled() {
		return getBatchSize() > 0;
	}

	protected boolean doNotSendChunks(boolean resultSetIsEmpty) {
		return resultSetIsEmpty || isBatchingDisabled();
	}

	public void sendResults(Iterable<?> result) {

		ResultSender<Object> resultSender = getResultSender();

		if (doNotSendChunks(!result.iterator().hasNext())) {
			resultSender.lastResult(result);
		}
		else {

			int batchSize = getBatchSize();

			List<Object> chunk = new ArrayList<>(batchSize);

			for (Iterator<?> it = result.iterator(); it.hasNext(); ) {

				if (chunk.size() < batchSize) {
					chunk.add(it.next());
				}

				if (chunk.size() == batchSize || !it.hasNext()) {

					if (it.hasNext()) {
						resultSender.sendResult(chunk);
					}
					else {
						resultSender.lastResult(chunk);
					}

					chunk.clear();
				}
			}
		}
	}

	public void sendArrayResults(Object result) {

		Assert.isTrue(ObjectUtils.isArray(result),
			() -> String.format("Object must be an array; was [%s]", ObjectUtils.nullSafeClassName(result)));

		int arrayLength = Array.getLength(result);

		ResultSender<Object> resultSender = getResultSender();

		if (doNotSendChunks(arrayLength == 0)) {
			resultSender.lastResult(result);
		}
		else {

			int batchSize = getBatchSize();

			for (int from = 0; from < arrayLength; from += batchSize) {

				int to = Math.min(arrayLength, from + batchSize);

				Object chunk = copyOfRange(result, from, to);

				if (to == arrayLength) {
					resultSender.lastResult(chunk);
				}
				else {
					resultSender.sendResult(chunk);
				}
			}
		}
	}

	private Object copyOfRange(Object result, int from, int to) {

		Class<?> resultType = result.getClass();

		if (boolean[].class.isAssignableFrom(resultType)) {
			return Arrays.copyOfRange((boolean[]) result, from, to);
		}
		else if (byte[].class.isAssignableFrom(resultType)) {
			return Arrays.copyOfRange((byte[]) result, from, to);
		}
		else if (short[].class.isAssignableFrom(resultType)) {
			return Arrays.copyOfRange((short[]) result, from, to);
		}
		else if (int[].class.isAssignableFrom(resultType)) {
			return Arrays.copyOfRange((int[]) result, from, to);
		}
		else if (long[].class.isAssignableFrom(resultType)) {
			return Arrays.copyOfRange((long[]) result, from, to);
		}
		else if (float[].class.isAssignableFrom(resultType)) {
			return Arrays.copyOfRange((float[]) result, from, to);
		}
		else if (double[].class.isAssignableFrom(resultType)) {
			return Arrays.copyOfRange((double[]) result, from, to);
		}
		else if (char[].class.isAssignableFrom(resultType)) {
			return Arrays.copyOfRange((char[]) result, from, to);
		}
		else {
			return Arrays.copyOfRange((Object[]) result, from, to);
		}
	}
}

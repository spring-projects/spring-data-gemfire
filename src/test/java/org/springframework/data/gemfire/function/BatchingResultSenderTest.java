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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.geode.cache.execute.ResultSender;
import org.junit.Test;

/**
 * @author David Turanski
 *
 */
public class BatchingResultSenderTest {

	@Test
	public void testArrayChunking() {

		testBatchingResultSender(new TestArrayResultSender(),1);
		testBatchingResultSender(new TestArrayResultSender(),0);
		testBatchingResultSender(new TestArrayResultSender(),9);
		testBatchingResultSender(new TestArrayResultSender(),10);
		testBatchingResultSender(new TestArrayResultSender(),1000);
	}


	@Test
	public void testListChunking() {
		testBatchingResultSender(new TestListResultSender(),1);
		testBatchingResultSender(new TestListResultSender(),0);
		testBatchingResultSender(new TestListResultSender(),9);
		testBatchingResultSender(new TestListResultSender(),10);
		testBatchingResultSender(new TestListResultSender(),1000);
	}

	private void testBatchingResultSender(AbstractTestResultSender resultSender, int batchSize){
		BatchingResultSender brs = new BatchingResultSender(batchSize, resultSender);

		List<Integer> result = new ArrayList<Integer>();
		for (int i = 0; i< 100; i++) {
			result.add(i);
		}
		//TODO: Clean this up. Ok for test code
		if (resultSender instanceof TestArrayResultSender) {
			brs.sendArrayResults(result.toArray(new Integer[100]));
		} else {
			brs.sendResults(result);
		}

		assertEquals(100,resultSender.getResults().size());

		for(int i=0; i< 100; i++) {
			assertEquals(i,resultSender.getResults().get(i));
		}

	}

	public static abstract class AbstractTestResultSender implements ResultSender<Object> {
		private List<Object> results = new ArrayList<Object>();

		/* (non-Javadoc)
		 * @see org.apache.geode.cache.execute.ResultSender#lastResult(java.lang.Object)
		 */
		@Override
		public void lastResult(Object arg0) {
			if (arg0 == null) {
				return;
			}
			addResults(arg0, results);
		}

		/* (non-Javadoc)
		 * @see org.apache.geode.cache.execute.ResultSender#sendException(java.lang.Throwable)
		 */
		@Override
		public void sendException(Throwable arg0) {
			fail();

		}

		/* (non-Javadoc)
		 * @see org.apache.geode.cache.execute.ResultSender#sendResult(java.lang.Object)
		 */
		@Override
		public void sendResult(Object arg0) {
			if (arg0 == null) {
				return;
			}
			addResults(arg0, results);
		}

		protected abstract void addResults(Object item, List<Object> results);

		public List<Object> getResults() {
			return this.results;
		}


	}

	public static class TestArrayResultSender extends AbstractTestResultSender {

		protected void addResults(Object arg0, List<Object> results) {
			assertTrue(arg0.getClass().isArray());
			Object[] array = (Object[]) arg0;
 			for (Object obj: array) {
				results.add(obj);
			}
		}
	}

	public static class TestListResultSender extends AbstractTestResultSender {
		protected void addResults(Object arg0, List<Object> results) {
			if (arg0 == null) {
				return;
			}
			assertTrue(arg0 instanceof Collection);
			Collection<?> list  = (Collection<?>) arg0;
			results.addAll(list);
		}
	}
}

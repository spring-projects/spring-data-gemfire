/*
 * Copyright 2002-2013 the original author or authors.
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
 * @author Udo Kohlmeyer
 *
 */
public class BatchingResultSenderTest {

	@Test
	public void testArrayChunking() {

		testBatchingResultSender(new TestArrayResultSender(),1);
		testBatchingResultSender(new TestArrayResultSender(),0);
		testBatchingResultSender(new TestArrayResultSender(),9);
		testBatchingResultSender(new TestArrayResultSender(),10,99);
		testBatchingResultSender(new TestArrayResultSender(),10,100);
		testBatchingResultSender(new TestArrayResultSender(),10,101);
		testBatchingResultSender(new TestArrayResultSender(),1000);

		testBatchingResultSender(new TestArrayResultSender(),2,0);
		testBatchingResultSender(new TestArrayResultSender(),0,0);
	}


	@Test
	public void testListChunking() {
		testBatchingResultSender(new TestListResultSender(),1);
		testBatchingResultSender(new TestListResultSender(),0);
		testBatchingResultSender(new TestListResultSender(),9);
		testBatchingResultSender(new TestListResultSender(),10,99);
		testBatchingResultSender(new TestListResultSender(),10,100);
		testBatchingResultSender(new TestListResultSender(),10,101);
		testBatchingResultSender(new TestListResultSender(),1000);

		testBatchingResultSender(new TestListResultSender(),3,0);
		testBatchingResultSender(new TestListResultSender(),0,0);
	}

    private void testBatchingResultSender(AbstractTestResultSender resultSender, int batchSize,int resultSetSize){
        BatchingResultSender brs = new BatchingResultSender(batchSize, resultSender);

        List<Integer> result = new ArrayList<>();
        for (int i = 0; i< resultSetSize; i++) {
            result.add(i);
        }
        //TODO: Clean this up. Ok for test code
        if (resultSender instanceof TestArrayResultSender) {
            brs.sendArrayResults(result.toArray(new Integer[resultSetSize]));
        } else {
            brs.sendResults(result);
        }

        assertEquals(resultSetSize,resultSender.getResults().size());

        assertTrue(resultSender.isLastResultSent());

        for(int i=0; i< resultSetSize; i++) {
            assertEquals(i,resultSender.getResults().get(i));
        }

    }

	private void testBatchingResultSender(AbstractTestResultSender resultSender, int batchSize){
		testBatchingResultSender(resultSender,batchSize,100);
	}

	public static abstract class AbstractTestResultSender implements ResultSender<Object> {
		private List<Object> results = new ArrayList<Object>();
		private boolean lastResultSent = false;

        public boolean isLastResultSent() {
            return lastResultSent;
        }

        /* (non-Javadoc)
		 * @see org.apache.geode.cache.execute.ResultSender#lastResult(java.lang.Object)
		 */
		@Override
		public void lastResult(Object arg0) {
		    lastResultSent = true;
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

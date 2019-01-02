/*
 * Copyright 2002-2019 the original author or authors.
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.IntStream;

import org.apache.geode.cache.execute.ResultSender;
import org.assertj.core.api.Assertions;
import org.junit.Test;

/**
 * Unit tests for {@link BatchingResultSender}.
 *
 * @author David Turanski
 * @author Udo Kohlmeyer
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.execute.ResultSender
 * @see org.springframework.data.gemfire.function.BatchingResultSender
 * @since 1.3.0
 */
public class BatchingResultSenderTest {

	@Test
	@SuppressWarnings("unchecked")
	public void constructBatchingResultSender() {

		ResultSender<Object> mockResultSender = mock(ResultSender.class);

		BatchingResultSender batchResultSender = new BatchingResultSender(20, mockResultSender);

		assertThat(batchResultSender).isNotNull();
		assertThat(batchResultSender.getBatchSize()).isEqualTo(20);
		assertThat(batchResultSender.getResultSender()).isEqualTo(mockResultSender);
	}

	@SuppressWarnings("unchecked")
	@Test(expected = IllegalArgumentException.class)
	public void constructBatchingResultSenderWithBatchSizeOfMinusOne() {

		try {
			new BatchingResultSender(-1, mock(ResultSender.class));
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("batchSize must be greater than equal to 0");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void constructBatchingResultSenderWithNullResultSender() {

		try {
			new BatchingResultSender(0, null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("ResultSender must not be null");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@SuppressWarnings("unchecked")
	@Test(expected = IllegalArgumentException.class)
	public void sendArrayResultsWithNonArrayThrowIllegalArgumentException() {

		try {
			new BatchingResultSender(20, mock(ResultSender.class)).sendArrayResults(new Object());
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("Object must be an array; was [java.lang.Object]");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void arrayChunkingIsCorrectWhenBatchSizeIsZero() {

		testBatchingResultSender(new TestArrayResultSender(),0); // Default result set size is 100
		testBatchingResultSender(new TestArrayResultSender(),0,1);
		testBatchingResultSender(new TestArrayResultSender(),0,2);
	}

	@Test
	public void arrayChunkingIsCorrectWhenBothBatchSizeAndResultSetSizeAreZero() {
		testBatchingResultSender(new TestArrayResultSender(),0,0);
	}

	@Test
	public void arrayCheckingIsCorrectWhenResultSetSizeIsZero() {

		testBatchingResultSender(new TestArrayResultSender(),1,0);
		testBatchingResultSender(new TestArrayResultSender(),2,0);
		testBatchingResultSender(new TestArrayResultSender(),3,0);
		testBatchingResultSender(new TestArrayResultSender(),50,0);
	}

	@Test
	public void arrayChunkingIsCorrect() {

		testBatchingResultSender(new TestArrayResultSender(),1);
		testBatchingResultSender(new TestArrayResultSender(),9);
		testBatchingResultSender(new TestArrayResultSender(),10,99);
		testBatchingResultSender(new TestArrayResultSender(),10,100);
		testBatchingResultSender(new TestArrayResultSender(),10,101);
		testBatchingResultSender(new TestArrayResultSender(),1000);
	}

	@Test
	public void listChunkingIsCorrectWhenBatchSizeIsZero() {

		testBatchingResultSender(new TestListResultSender(),0); // Default result set size is 100
		testBatchingResultSender(new TestListResultSender(),0, 1);
		testBatchingResultSender(new TestListResultSender(),0, 2);
	}

	@Test
	public void listChunkingIsCorrectWhenBothBatchSizeAndResultSetSizeAreZero() {

		testBatchingResultSender(new TestListResultSender(),0,0);
	}

	@Test
	public void listChunkingIsCorrectWhenResultSetSizeIsZero() {

		testBatchingResultSender(new TestListResultSender(),1,0);
		testBatchingResultSender(new TestListResultSender(),2,0);
		testBatchingResultSender(new TestListResultSender(),3,0);
		testBatchingResultSender(new TestListResultSender(),50,0);
	}

	@Test
	public void listChunkingIsCorrect() {

		testBatchingResultSender(new TestListResultSender(),1);
		testBatchingResultSender(new TestListResultSender(),9);
		testBatchingResultSender(new TestListResultSender(),10,99);
		testBatchingResultSender(new TestListResultSender(),10,100);
		testBatchingResultSender(new TestListResultSender(),10,101);
		testBatchingResultSender(new TestListResultSender(),1000);
	}

    private void testBatchingResultSender(AbstractTestResultSender resultSender, int batchSize, int resultSetSize){

        BatchingResultSender batchResultSender = new BatchingResultSender(batchSize, resultSender);

        List<Integer> result = new ArrayList<>();

        IntStream.range(0, resultSetSize).forEach(result::add);

        if (resultSender instanceof TestArrayResultSender) {
            batchResultSender.sendArrayResults(result.toArray(new Integer[resultSetSize]));
        } else {
            batchResultSender.sendResults(result);
        }

        assertThat(resultSender.isLastResultSent()).isTrue();
        assertThat(resultSender.getResults()).hasSize(resultSetSize);

        IntStream.range(0, resultSetSize).forEach(index ->
			assertThat(resultSender.getResults().get(index)).isEqualTo(index));
    }

	private void testBatchingResultSender(AbstractTestResultSender resultSender, int batchSize){
		testBatchingResultSender(resultSender,batchSize,100);
	}

	public static abstract class AbstractTestResultSender implements ResultSender<Object> {

		private boolean lastResultSent = false;

		private List<Object> results = new ArrayList<>();

        public boolean isLastResultSent() {
            return this.lastResultSent;
        }

		@Override
		public void lastResult(Object result) {

		    this.lastResultSent = true;

			Optional.ofNullable(result)
				.ifPresent(it -> addResults(it, this.results));
		}

		@Override
		public void sendException(Throwable cause) {
			Assertions.fail("Function send result operation failed", cause);
		}

		@Override
		public void sendResult(Object result) {

			Optional.ofNullable(result)
				.ifPresent(it -> addResults(it, this.results));
		}

		protected abstract void addResults(Object item, List<Object> results);

		public List<Object> getResults() {
			return this.results;
		}
	}

	public static class TestArrayResultSender extends AbstractTestResultSender {

		protected void addResults(Object result, List<Object> results) {

			assertThat(result.getClass().isArray()).isTrue();

			Object[] array = (Object[]) result;

			Collections.addAll(results, array);
		}
	}

	public static class TestListResultSender extends AbstractTestResultSender {

		protected void addResults(Object result, List<Object> results) {

			assertThat(result).isInstanceOf(Collection.class);

			Collection<?> list = (Collection<?>) result;

			results.addAll(list);
		}
	}
}

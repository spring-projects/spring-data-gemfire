/*
 * Copyright 2011-2012-2012 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.listener.adapter;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import org.junit.Before;
import org.junit.Test;
import org.springframework.data.gemfire.listener.ContinuousQueryListener;

import com.gemstone.gemfire.cache.Operation;
import com.gemstone.gemfire.cache.query.CqEvent;
import com.gemstone.gemfire.cache.query.CqQuery;
import com.gemstone.gemfire.cache.query.internal.CqQueryImpl;

/**
 * @author Costin Leau
 * @author Oliver Gierke
 */
public class QueryListenerAdapterTest {

	private ContinuousQueryListenerAdapter adapter;

	@Before
	public void setUp() {
		adapter = new ContinuousQueryListenerAdapter();
	}

	CqEvent event() {
		CqEvent event = new CqEvent() {
			final CqQuery cq = new CqQueryImpl();
			final byte[] ba = new byte[0];
			final Object key = new Object();
			final Object value = new Object();
			final Exception ex = new Exception();

			public Operation getBaseOperation() {
				return Operation.CACHE_CLOSE;
			}

			public CqQuery getCq() {
				return cq;
			}

			public byte[] getDeltaValue() {
				return ba;
			}

			public Object getKey() {
				return key;
			}

			public Object getNewValue() {
				return value;
			}

			public Operation getQueryOperation() {
				return Operation.CACHE_CREATE;
			}

			public Throwable getThrowable() {
				return ex;
			}
		};

		return event;
	}

	public static interface Delegate {
		void handleEvent(CqEvent event);

		void handleQuery(CqQuery query);

		void handleOperation(Operation op);

		void handleArray(byte[] ba);

		void handleKey(Object key);

		void handleKV(Object k, Object v);

		void handleEx(Throwable th);

		void handleOps(Operation base, Operation query);

		void handleAll(CqEvent event, CqQuery query, byte[] ba, Object key, Operation op, Throwable th, Operation qOp,
				Object v);

		void handleInvalid(Object o1, Object o2, Object o3);
	}

	@Test
	public void testThatWhenNoDelegateIsSuppliedTheDelegateIsAssumedToBeTheListenerAdapterItself() throws Exception {
		assertSame(adapter, adapter.getDelegate());
	}

	@Test
	public void testThatTheDefaultHandlingMethodNameIsTheConstantDefault() throws Exception {
		assertEquals(ContinuousQueryListenerAdapter.ORIGINAL_DEFAULT_LISTENER_METHOD, adapter.getDefaultListenerMethod());
	}

	@Test
	public void testAdapterWithListenerAndDefaultMessage() throws Exception {
		ContinuousQueryListener mock = mock(ContinuousQueryListener.class);

		ContinuousQueryListenerAdapter adapter = new ContinuousQueryListenerAdapter(mock);
		CqEvent event = event();
		adapter.onEvent(event);
		verify(mock).onEvent(event);
	}

	@Test
	public void testHandleEvent() throws Exception {
		Delegate mock = mock(Delegate.class);
		ContinuousQueryListenerAdapter adapter = new ContinuousQueryListenerAdapter(mock);
		CqEvent event = event();

		adapter.onEvent(event);
		verify(mock).handleEvent(event);
	}

	@Test
	public void testHandleArray() throws Exception {
		Delegate mock = mock(Delegate.class);
		ContinuousQueryListenerAdapter adapter = new ContinuousQueryListenerAdapter(mock);
		adapter.setDefaultListenerMethod("handleArray");
		CqEvent event = event();
		adapter.onEvent(event);
		verify(mock).handleArray(event.getDeltaValue());
	}

	@Test
	public void testHandleKey() throws Exception {
		Delegate mock = mock(Delegate.class);
		ContinuousQueryListenerAdapter adapter = new ContinuousQueryListenerAdapter(mock);
		adapter.setDefaultListenerMethod("handleKey");
		CqEvent event = event();

		adapter.onEvent(event);
		verify(mock).handleKey(event.getKey());
	}

	@Test
	public void testHandleKV() throws Exception {
		Delegate mock = mock(Delegate.class);
		ContinuousQueryListenerAdapter adapter = new ContinuousQueryListenerAdapter(mock);
		adapter.setDefaultListenerMethod("handleKV");
		CqEvent event = event();

		adapter.onEvent(event);
		verify(mock).handleKV(event.getKey(), event.getNewValue());
	}

	@Test
	public void testHandleEx() throws Exception {
		Delegate mock = mock(Delegate.class);
		ContinuousQueryListenerAdapter adapter = new ContinuousQueryListenerAdapter(mock);
		adapter.setDefaultListenerMethod("handleEx");
		CqEvent event = event();

		adapter.onEvent(event);
		verify(mock).handleEx(event.getThrowable());
	}

	@Test
	public void testHandleOps() throws Exception {
		Delegate mock = mock(Delegate.class);
		ContinuousQueryListenerAdapter adapter = new ContinuousQueryListenerAdapter(mock);
		adapter.setDefaultListenerMethod("handleOps");
		CqEvent event = event();

		adapter.onEvent(event);
		verify(mock).handleOps(event.getBaseOperation(), event.getQueryOperation());
	}

	@Test
	public void testHandleAll() throws Exception {
		Delegate mock = mock(Delegate.class);
		ContinuousQueryListenerAdapter adapter = new ContinuousQueryListenerAdapter(mock);
		adapter.setDefaultListenerMethod("handleAll");
		CqEvent event = event();

		adapter.onEvent(event);
		verify(mock).handleAll(event, event.getCq(), event.getDeltaValue(), event.getKey(), event.getBaseOperation(),
				event.getThrowable(), event.getQueryOperation(), event.getNewValue());
	}

	@Test
	public void testInvalid() throws Exception {
		Delegate mock = mock(Delegate.class);
		ContinuousQueryListenerAdapter adapter = new ContinuousQueryListenerAdapter(mock);
		adapter.setDefaultListenerMethod("handleInvalid");

		adapter.onEvent(event());
		doThrow(new IllegalArgumentException()).when(mock);
	}

	/**
	 * @see SGF-89
	 */
	@Test
	public void triggersListenerImplementingInterfaceCorrectly() {

		SampleListener listener = new SampleListener();

		ContinuousQueryListener listenerAdapter = new ContinuousQueryListenerAdapter(listener) {
			protected void handleListenerException(Throwable ex) {
				throw new RuntimeException(ex);
			}
		};

		listenerAdapter.onEvent(event());
		assertThat(listener.count, is(1));
	}

	class SampleListener implements ContinuousQueryListener {

		int count;

		@Override
		public void onEvent(CqEvent event) {
			count++;
		}
	}
}
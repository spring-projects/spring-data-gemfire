/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FilenameFilter;
import java.util.List;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.Test;
import org.springframework.data.gemfire.RecreatingContextTest;

import com.gemstone.gemfire.cache.asyncqueue.AsyncEvent;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;

/**
 * This test is only valid for GF 7.0 and above
 * 
 * @author David Turanski
 * 
 */

public class GemfireAsyncEventQueueNamespaceTest extends RecreatingContextTest {

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.RecreatingContextTest#location()
	 */
	@Override
	protected String location() {
		return "/org/springframework/data/gemfire/config/async-event-queue-ns.xml";
	}

	@Override
	protected void configureContext() {
		//ctx.getBeanFactory().addBeanPostProcessor(new GemfireTestBeanPostProcessor());
	}

	@Before
	@Override
	public void createCtx() {
		if (ParsingUtils.GEMFIRE_VERSION.startsWith("7")) {
			super.createCtx();
		}
	}

	@AfterClass
	public static void tearDown() {
		for (String name : new File(".").list(new FilenameFilter() {

			@Override
			public boolean accept(File dir, String name) {
				return name.startsWith("BACKUP");
			}
		})) {
			new File(name).delete();
		}
	}

	/**
	 * 
	 */
	@Test
	public void testAsyncEventQueue() {
		AsyncEventQueue aseq = ctx.getBean("async-event-queue", AsyncEventQueue.class);
		assertEquals(10, aseq.getBatchSize());
		assertTrue(aseq.isPersistent());
		assertEquals("diskstore", aseq.getDiskStoreName());
		assertEquals(50, aseq.getMaximumQueueMemory());
	}

	@SuppressWarnings("rawtypes")
	public static class TestAsyncEventListener implements AsyncEventListener {

		@Override
		public void close() {
			// TODO Auto-generated method stub
		}

		@Override
		public boolean processEvents(List<AsyncEvent> arg0) {
			// TODO Auto-generated method stub
			return false;
		}

	}

}

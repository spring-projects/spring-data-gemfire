/*
 * Copyright 2010-2012 the original author or authors.
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

import javax.annotation.Resource;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.TransactionEvent;
import com.gemstone.gemfire.cache.TransactionListener;
import com.gemstone.gemfire.cache.TransactionWriter;
import com.gemstone.gemfire.cache.TransactionWriterException;

/**
 * @author David Turanski
 * 
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("tx-listeners-and-writers.xml")
public class TxEventHandlersTest {
	@Autowired
	TestListener txListener1;

	@Autowired
	TestListener txListener2;

	@Autowired
	TestWriter txWriter;

	@Resource(name = "gemfireCache")
	Cache cache;

	@SuppressWarnings("rawtypes")
	@Resource(name = "local")
	Region local;

	@Test
	public void test() throws Exception {
		cache.getCacheTransactionManager().begin();
		local.put("hello", "world");
		cache.getCacheTransactionManager().commit();
		assertEquals("txListener1", txListener1.value);
		assertEquals("txListener2", txListener2.value);
		assertEquals("txWriter", txWriter.value);

	}

	public static class TestListener implements TransactionListener, BeanNameAware {

		private String name;

		public String value;

		public boolean closed;

		public boolean afterCommit;

		@Override
		public void close() {
			closed = true;

		}

		@Override
		public void afterCommit(TransactionEvent arg0) {
			afterCommit = true;
			value = name;
		}

		@Override
		public void afterFailedCommit(TransactionEvent arg0) {
			// TODO Auto-generated method stub

		}

		@Override
		public void afterRollback(TransactionEvent arg0) {
			// TODO Auto-generated method stub

		}

		@Override
		public void setBeanName(String name) {
			this.name = name;
		};
	}

	public static class TestWriter implements TransactionWriter, BeanNameAware {

		private String name;

		public String value;

		@Override
		public void close() {
			// TODO Auto-generated method stub

		}

		@Override
		public void beforeCommit(TransactionEvent arg0) throws TransactionWriterException {
			this.value = name;

		}

		@Override
		public void setBeanName(String name) {
			this.name = name;
		};

	}
}

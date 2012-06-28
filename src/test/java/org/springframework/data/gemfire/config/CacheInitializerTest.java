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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import java.util.Properties;

import javax.annotation.Resource;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.Declarable;

/**
 * @author David Turanski
 * 
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("cache-with-initializer.xml")
public class CacheInitializerTest {
	@Autowired
	TestInitializer cacheInitializer;

	@Resource(name = "gemfire-cache")
	Cache cache;

	@Test
	public void test() throws Exception {
		assertNotNull(cache.getInitializer());
		assertSame(cacheInitializer, cache.getInitializer());
		// assertEquals("cacheInitializer", cacheInitializer.value);

	}

	public static class TestInitializer implements Declarable, BeanNameAware {

		private String name;

		public String value;

		private String first;

		private String last;

		@Override
		public void setBeanName(String name) {
			this.name = name;
		}

		public String getFirst() {
			return first;
		}

		public void setFirst(String first) {
			this.first = first;
		}

		public String getLast() {
			return last;
		}

		public void setLast(String last) {
			this.last = last;
		}

		@Override
		public void init(Properties props) {
			this.value = this.name;
		};
	}

}

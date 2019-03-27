/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.access.BeanFactoryReference;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Costin Leau
 * @author John Blum
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { "locatorContext.xml" })
@SuppressWarnings("unused")
public class GemfireBeanFactoryLocatorTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	@Autowired
	private ApplicationContext applicationContext;

	private GemfireBeanFactoryLocator locator1, locator2;

	private String INSTANCE_1 = "instance1";
	private String INSTANCE_2 = "instance2";

	@Before
	public void init() {
		locator1 = new GemfireBeanFactoryLocator();
		locator1.setBeanName(INSTANCE_1);
		locator1.setBeanFactory(applicationContext);
		locator1.afterPropertiesSet();

		locator2 = new GemfireBeanFactoryLocator();
		locator2.setBeanName(INSTANCE_2);
		locator2.setBeanFactory(applicationContext);
		locator2.afterPropertiesSet();
	}

	@After
	public void destroy() {
		BeanFactoryReference ref1;
		try {
			ref1 = locator1.useBeanFactory(INSTANCE_1);
			ref1.release();
			BeanFactoryReference ref2 = locator2.useBeanFactory(INSTANCE_2);
			ref2.release();

		} catch (IllegalArgumentException e) {
			// it's okay
		}
		locator1.destroy();
		locator2.destroy();
		locator1 = null;
		locator2 = null;
	}

	@Test
	public void testBeanFactoryRelease() throws Exception {
	}

	@Test
	public void testFactoryLocator() throws Exception {
		BeanFactoryReference reference1 = locator1.useBeanFactory(INSTANCE_1);
		BeanFactoryReference reference2 = locator2.useBeanFactory(INSTANCE_2);
		BeanFactoryReference aliasRef1 = locator1.useBeanFactory("alias1");
		BeanFactoryReference aliasRef2 = locator1.useBeanFactory("alias2");

		// verify the static map
		BeanFactory factory1 = reference1.getFactory();
		BeanFactory factory2 = reference2.getFactory();
		BeanFactory factory3 = reference2.getFactory();
		// get the alias from different factories
		BeanFactory alias1 = aliasRef1.getFactory();
		BeanFactory alias2 = aliasRef2.getFactory();

		assertSame(factory1, factory2);
		assertSame(factory1, factory3);
		// verify it's the same bean factory as the application context
		assertSame(factory1, applicationContext);

		// verify aliases
		assertSame(alias1, alias2);
		assertSame(factory1, alias1);

		aliasRef1.release();
		aliasRef2.release();
		reference1.release();
		reference2.release();
	}

	@Test
	public void testDefaultFactoryLocator() throws Exception {
		try {
			locator1.useBeanFactory(null);
			fail("there are more then one bean factories registered - should have thrown exception");
		} catch (IllegalArgumentException e) {
			// it's okay
		}
	}

	@Test
	public void factoryLocatorContract() throws Exception {
		BeanFactoryReference factory1 = locator1.useBeanFactory(INSTANCE_1);

		assertThat(factory1.getFactory(), is(notNullValue()));

		factory1.release();

		expectedException.expect(IllegalStateException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("The BeanFactory has already been released or closed");

		factory1.getFactory();
	}

}

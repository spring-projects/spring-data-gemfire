/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.expiration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.apache.geode.cache.ExpirationAction;
import org.apache.geode.cache.ExpirationAttributes;

import org.junit.Test;

/**
 * The ExpirationAttributesFactoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the ExpirationAttributesFactoryBean class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see ExpirationAttributesFactoryBean
 * @since 1.6.0
 */
public class ExpirationAttributesFactoryBeanTest {

	@Test
	public void testIsSingleton() {
		assertTrue(new ExpirationAttributesFactoryBean().isSingleton());
	}

	@Test
	public void testSetAndGetAction() {
		ExpirationAttributesFactoryBean expirationAttributesFactoryBean = new ExpirationAttributesFactoryBean();

		assertEquals(ExpirationAttributesFactoryBean.DEFAULT_EXPIRATION_ACTION,
			expirationAttributesFactoryBean.getAction());

		expirationAttributesFactoryBean.setAction(ExpirationAction.LOCAL_DESTROY);

		assertEquals(ExpirationAction.LOCAL_DESTROY, expirationAttributesFactoryBean.getAction());

		expirationAttributesFactoryBean.setAction(null);

		assertEquals(ExpirationAttributesFactoryBean.DEFAULT_EXPIRATION_ACTION,
			expirationAttributesFactoryBean.getAction());
	}

	@Test
	public void testSetAndGetTimeout() {
		ExpirationAttributesFactoryBean expirationAttributesFactoryBean = new ExpirationAttributesFactoryBean();

		assertEquals(0, expirationAttributesFactoryBean.getTimeout());

		expirationAttributesFactoryBean.setTimeout(60000);

		assertEquals(60000, expirationAttributesFactoryBean.getTimeout());

		expirationAttributesFactoryBean.setTimeout(null);

		assertEquals(0, expirationAttributesFactoryBean.getTimeout());
	}

	@Test
	public void testAfterPropertiesSet() throws Exception {
		ExpirationAttributesFactoryBean expirationAttributesFactoryBean = new ExpirationAttributesFactoryBean();

		assertNull(expirationAttributesFactoryBean.getObject());
		assertEquals(ExpirationAttributes.class, expirationAttributesFactoryBean.getObjectType());

		expirationAttributesFactoryBean.setAction(ExpirationAction.DESTROY);
		expirationAttributesFactoryBean.setTimeout(8192);
		expirationAttributesFactoryBean.afterPropertiesSet();

		ExpirationAttributes expirationAttributes = expirationAttributesFactoryBean.getObject();

		assertNotNull(expirationAttributes);
		assertEquals(ExpirationAction.DESTROY, expirationAttributes.getAction());
		assertEquals(8192, expirationAttributes.getTimeout());
		assertEquals(expirationAttributes.getClass(), expirationAttributesFactoryBean.getObjectType());
	}

}

/*
 * Copyright 2011-2013 the original author or authors.
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
package org.springframework.data.gemfire.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.Test;
import org.springframework.data.gemfire.GemfireOperations;
import org.springframework.data.gemfire.GemfireTemplate;

/**
 * @author Costin Leau
 * @author John Blum
 */
public class GemfireDaoSupportTests {

	@Test
	@SuppressWarnings("rawtypes")
	public void testGemfireDaoSupportWithTemplate() throws Exception {
		final AtomicBoolean flag = new AtomicBoolean(false);

		GemfireDaoSupport dao = new GemfireDaoSupport() {
			protected void initDao() {
				flag.set(true);
			}
		};

		GemfireOperations expectedTemplate = new GemfireTemplate();

		dao.setGemfireTemplate(expectedTemplate);
		dao.afterPropertiesSet();

		assertNotNull("template not created", dao.getGemfireTemplate());
		assertEquals("incorrect template", expectedTemplate, dao.getGemfireTemplate());
		assertTrue("initDao not called", flag.get());
	}

	@Test(expected = IllegalStateException.class)
	public void testInvalidDaoTemplate() throws Exception {
		new GemfireDaoSupport().afterPropertiesSet();
	}

}

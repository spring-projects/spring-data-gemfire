/*
 * Copyright 2011-2013 the original author or authors.
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
package org.springframework.data.gemfire.support;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.springframework.data.gemfire.GemfireTemplate;

/**
 * @author Costin Leau
 * 
 */
public class GemfireDaoSupportTests extends TestCase {

	public void testGemfireDaoSupportWithTemplate() throws Exception {
		GemfireTemplate template = new GemfireTemplate();
		final List test = new ArrayList();
		GemfireDaoSupport dao = new GemfireDaoSupport() {
			protected void initDao() {
				test.add("test");
			}
		};
		dao.setGemfireTemplate(template);
		dao.afterPropertiesSet();
		assertNotNull("template not created", dao.getGemfireTemplate());
		assertEquals("incorrect template", template, dao.getGemfireTemplate());
		assertEquals("initDao not called", test.size(), 1);
	}

	public void testInvalidDaoTemplate() throws Exception {
		GemfireDaoSupport dao = new GemfireDaoSupport() {
		};
		try {
			dao.afterPropertiesSet();
			fail("expected exception");
		} catch (IllegalArgumentException iae) {
			// okay
		}
	}
}
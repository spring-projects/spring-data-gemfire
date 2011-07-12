/*
 * Copyright 2011 the original author or authors.
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

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;
import org.springframework.dao.InvalidDataAccessApiUsageException;

import com.gemstone.gemfire.cache.query.SelectResults;

/**
 * @author Costin Leau
 */
public class GemfireTemplateTest extends RecreatingContextTest {

	private static final String MULTI_QUERY = "select * from /simple";
	private static final String SINGLE_QUERY = "(select * from /simple).size";

	@Override
	protected String location() {
		return "org/springframework/data/gemfire/basic-template.xml";
	}

	@Test
	public void testFind() throws Exception {
		GemfireTemplate template = ctx.getBean("template", GemfireTemplate.class);
		SelectResults<Object> find = template.find(MULTI_QUERY);
		assertNotNull(find);
	}

	@Test
	public void testFindUnique() throws Exception {
		GemfireTemplate template = ctx.getBean("template", GemfireTemplate.class);
		Integer find = template.findUnique(SINGLE_QUERY);
		assertEquals(find, Integer.valueOf(0));
	}

	@Test(expected = InvalidDataAccessApiUsageException.class)
	public void testFindMultiException() throws Exception {
		GemfireTemplate template = ctx.getBean("template", GemfireTemplate.class);
		SelectResults<Object> find = template.find(SINGLE_QUERY);
	}

	@Test(expected = InvalidDataAccessApiUsageException.class)
	public void testFindMultiOne() throws Exception {
		GemfireTemplate template = ctx.getBean("template", GemfireTemplate.class);
		SelectResults<Object> find = template.findUnique(MULTI_QUERY);
	}
}
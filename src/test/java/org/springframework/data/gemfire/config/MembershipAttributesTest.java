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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.springframework.data.gemfire.RecreatingContextTest;

import com.gemstone.gemfire.cache.LossAction;
import com.gemstone.gemfire.cache.MembershipAttributes;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.ResumptionAction;
import com.gemstone.gemfire.distributed.Role;

/**
 * @author David Turanski
 * 
 */
public class MembershipAttributesTest extends RecreatingContextTest {

	@Override
	protected String location() {
		return "org/springframework/data/gemfire/config/membership-attributes-ns.xml";
	}

	@Test
	public void testMembershipAttributes() {
		Region<?, ?> simple = ctx.getBean("simple", Region.class);
		MembershipAttributes ma = simple.getAttributes().getMembershipAttributes();
		assertFalse(ma.hasRequiredRoles());

		Region<?, ?> secure = ctx.getBean("secure", Region.class);
		ma = secure.getAttributes().getMembershipAttributes();
		assertTrue(ma.hasRequiredRoles());
		assertEquals(ResumptionAction.REINITIALIZE, ma.getResumptionAction());
		assertEquals(LossAction.LIMITED_ACCESS, ma.getLossAction());
		for (Role role : ma.getRequiredRoles()) {
			assertTrue("ROLE1".equals(role.getName()) || "ROLE2".equals(role.getName()));
		}
	}
}

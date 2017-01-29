/*
 * Copyright 2010-2018 the original author or authors.
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
package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.apache.geode.cache.LossAction;
import org.apache.geode.cache.MembershipAttributes;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.ResumptionAction;
import org.apache.geode.distributed.Role;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author David Turanski
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations= "/org/springframework/data/gemfire/config/xml/membership-attributes-ns.xml",
	initializers=GemfireTestApplicationContextInitializer.class)
public class MembershipAttributesTest {

    @Autowired ApplicationContext ctx;

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

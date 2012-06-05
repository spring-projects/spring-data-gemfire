package org.springframework.data.gemfire;

import org.junit.Test;

public class SubRegionFactoryBeanTest {
	@Test
	public void test() throws Exception {
		SubRegionFactoryBean srfb = new SubRegionFactoryBean();
		srfb.setName("child");
		srfb.afterPropertiesSet();
		SubRegion sr = srfb.getObject();
	}
}

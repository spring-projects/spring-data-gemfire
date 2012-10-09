package org.springframework.data.gemfire.client;

import static org.junit.Assert.*;

import javax.annotation.Resource;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("client-cache.xml")
public class ClientCacheTest {
	@Resource(name="challengeQuestionsRegion")
	Region<?,?> region;
	
	@Autowired
	ClientCache cache;
	
    @Test
    public void test() {
    	assertEquals("gemfirePool",region.getAttributes().getPoolName());
    }
}

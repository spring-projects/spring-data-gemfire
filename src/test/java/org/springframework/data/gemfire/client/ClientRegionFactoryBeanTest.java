package org.springframework.data.gemfire.client;

import static org.junit.Assert.assertSame;

import org.junit.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.BeanFactory;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientRegionFactory;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;
import com.gemstone.gemfire.cache.client.Pool;

public class ClientRegionFactoryBeanTest {

	@Test
	public void testLookupFallbackFailingToUseProvidedShortcut()
			throws Exception {
		ClientRegionFactoryBean<Object, Object> fb = new ClientRegionFactoryBean<Object, Object>();
		fb.setShortcut(ClientRegionShortcut.CACHING_PROXY);
		
		Pool pool = Mockito.mock(Pool.class);
		
		BeanFactory beanFactory = Mockito.mock(BeanFactory.class);
		
		Mockito.when(beanFactory.getBean(Pool.class)).thenReturn(pool);
		
		fb.setBeanFactory(beanFactory);

		String regionName = "regionName";
		ClientCache cache = Mockito.mock(ClientCache.class);

		@SuppressWarnings("unchecked")
		ClientRegionFactory<Object, Object> factory = Mockito
				.mock(ClientRegionFactory.class);
		Mockito.when(
				cache.createClientRegionFactory(ClientRegionShortcut.CACHING_PROXY))
				.thenReturn(factory);

		@SuppressWarnings("unchecked")
		Region<Object, Object> region = Mockito.mock(Region.class);
		Mockito.when(factory.create(regionName)).thenReturn(region);

		Region<Object, Object> result = fb.lookupFallback(cache, regionName);

		assertSame(region, result);
	}
}

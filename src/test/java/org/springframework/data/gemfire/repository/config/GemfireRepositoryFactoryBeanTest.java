package org.springframework.data.gemfire.repository.config;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.repository.support.GemfireRepositoryFactoryBean;
import org.springframework.data.mapping.context.MappingContext;

import com.gemstone.gemfire.cache.Region;

/**
 * The GemfireRepositoryFactoryBeanTest class is test suite of test cases testing the contract and functionality
 * of the GemfireRepositoryFactoryBean class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @since 1.6.3
 */
public class GemfireRepositoryFactoryBeanTest {

	private GemfireRepositoryFactoryBean repositoryFactoryBean;

	@Before
	public void setup() {
		repositoryFactoryBean = new GemfireRepositoryFactoryBean();
	}

	protected <T> List<T> toList(Iterable<T> iterable) {
		List<T> list = new ArrayList<T>();

		if (iterable != null) {
			for (T element : iterable) {
				list.add(element);
			}
		}

		return list;
	}

	@Test
	public void setApplicationContext() throws Exception {
		Map<String, Region> expectedRegions = new HashMap<String, Region>(2);

		expectedRegions.put("regionOne", mock(Region.class));
		expectedRegions.put("regionTwo", mock(Region.class));

		ApplicationContext mockApplicationContext = mock(ApplicationContext.class);

		when(mockApplicationContext.getBeansOfType(eq(Region.class))).thenReturn(expectedRegions);

		repositoryFactoryBean.setApplicationContext(mockApplicationContext);

		Iterable<Region> actualRegions = TestUtils.readField("regions", repositoryFactoryBean);

		assertThat(actualRegions, is(notNullValue()));

		List<Region> regions = toList(actualRegions);

		assertThat(regions.size(), is(equalTo(2)));
		assertThat(regions.containsAll(expectedRegions.values()), is(true));

		ApplicationContext actualApplicationContext = TestUtils.readField("applicationContext", repositoryFactoryBean);

		assertThat(actualApplicationContext, is(sameInstance(mockApplicationContext)));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void setGemfireMappingContextAlsoSetsMappingContext() throws Exception {
		MappingContext mockMappingContext = mock(MappingContext.class);

		repositoryFactoryBean.setGemfireMappingContext(mockMappingContext);

		MappingContext actualMappingContext = TestUtils.readField("context", repositoryFactoryBean);

		assertThat(actualMappingContext, is(sameInstance(mockMappingContext)));

		actualMappingContext = TestUtils.readField("mappingContext", repositoryFactoryBean);

		assertThat(actualMappingContext, is(sameInstance(mockMappingContext)));
	}

}

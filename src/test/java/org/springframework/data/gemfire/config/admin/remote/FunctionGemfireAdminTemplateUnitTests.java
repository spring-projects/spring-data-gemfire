/*
 * Copyright 2017-2018 the original author or authors.
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

package org.springframework.data.gemfire.config.admin.remote;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.util.ArrayUtils.asArray;
import static org.springframework.data.gemfire.util.CollectionUtils.asSet;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.Scope;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.execute.Function;
import org.apache.geode.cache.query.Index;
import org.apache.geode.management.internal.cli.domain.RegionInformation;
import org.apache.geode.management.internal.cli.functions.GetRegionsFunction;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.data.gemfire.client.function.ListRegionsOnServerFunction;
import org.springframework.data.gemfire.config.admin.functions.CreateIndexFunction;
import org.springframework.data.gemfire.config.admin.functions.CreateRegionFunction;
import org.springframework.data.gemfire.config.admin.functions.ListIndexesFunction;
import org.springframework.data.gemfire.config.schema.definitions.IndexDefinition;
import org.springframework.data.gemfire.config.schema.definitions.RegionDefinition;
import org.springframework.data.gemfire.function.execution.GemfireFunctionOperations;

/**
 * Unit tests for {@link FunctionGemfireAdminTemplate}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.admin.remote.FunctionGemfireAdminTemplate
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class FunctionGemfireAdminTemplateUnitTests {

	private FunctionGemfireAdminTemplate template;

	@Mock
	private ClientCache mockClientCache;

	@Mock
	private GemfireFunctionOperations mockFunctionOperations;

	@Mock
	private Index mockIndex;

	@Mock
	private Region mockRegion;

	@Before
	public void setup() {

		this.template = spy(new FunctionGemfireAdminTemplate(this.mockClientCache));

		doReturn(this.mockFunctionOperations).when(this.template)
			.newGemfireFunctionOperations(any(ClientCache.class));

		when(this.mockIndex.getName()).thenReturn("MockIndex");
		when(this.mockRegion.getName()).thenReturn("MockRegion");
	}

	private Region mockRegion(String name) {

		Region mockRegion = mock(Region.class, name);

		when(mockRegion.getFullPath()).thenReturn(String.format("%1$s%2$s", Region.SEPARATOR, name));

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class,
			String.format("Mock%sRegionAttributes", name));

		when(mockRegionAttributes.getDataPolicy()).thenReturn(DataPolicy.PARTITION);
		when(mockRegionAttributes.getScope()).thenReturn(Scope.DISTRIBUTED_ACK);
		when(mockRegion.getAttributes()).thenReturn(mockRegionAttributes);

		return mockRegion;
	}

	private RegionInformation newRegionInformation(String regionName) {
		return new RegionInformation(mockRegion(regionName), false);
	}

	@Test
	public void constructFunctionGemfireAdminTemplateWithClientCache() {

		FunctionGemfireAdminTemplate template = new FunctionGemfireAdminTemplate(this.mockClientCache);

		assertThat(template).isNotNull();
		assertThat(template.getClientCache()).isSameAs(this.mockClientCache);
	}

	@Test(expected = IllegalArgumentException.class)
	public void constructFunctionGemfireAdminTemplateWithNull() {

		try {
			new FunctionGemfireAdminTemplate(null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("ClientCache is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void getAvailableServerRegionsExecutesListRegionsOnServerFunction() {

		when(this.mockFunctionOperations.executeAndExtract(any(Function.class)))
			.thenReturn(asSet("RegionOne", "RegionTwo"));

		Iterable<String> availableServerRegions = this.template.getAvailableServerRegions();

		assertThat(availableServerRegions).isNotNull();
		assertThat(availableServerRegions).hasSize(2);
		assertThat(availableServerRegions).contains("RegionOne", "RegionTwo");

		verify(this.mockFunctionOperations, times(1))
			.executeAndExtract(isA(ListRegionsOnServerFunction.class));
	}

	@Test
	public void getAvailableServerRegionsExecutesGetRegionsFunction() {

		when(this.mockFunctionOperations.executeAndExtract(isA(ListRegionsOnServerFunction.class)))
			.thenThrow(new RuntimeException("TEST"));

		Object[] regionInformation = asArray(newRegionInformation("MockRegionOne"),
			newRegionInformation("MockRegionTwo"));

		when(this.mockFunctionOperations.executeAndExtract(isA(GetRegionsFunction.class)))
			.thenReturn(regionInformation);

		Iterable<String> availableServerRegions = this.template.getAvailableServerRegions();

		assertThat(availableServerRegions).isNotNull();
		assertThat(availableServerRegions).hasSize(2);
		assertThat(availableServerRegions).contains("MockRegionOne", "MockRegionTwo");

		verify(this.mockFunctionOperations, times(1))
			.executeAndExtract(isA(ListRegionsOnServerFunction.class));

		verify(this.mockFunctionOperations, times(1))
			.executeAndExtract(isA(GetRegionsFunction.class));
	}

	@Test
	public void getAvailableServerRegionIndexesCallsExecuteWithListIndexesFunctionId() {

		this.template.getAvailableServerRegionIndexes();

		verify(this.mockFunctionOperations, times(1))
			.executeAndExtract(eq(ListIndexesFunction.LIST_INDEXES_FUNCTION_ID));
	}

	@Test
	public void createRegionCallsExecuteWithCreateRegionFunctionIdAndRegionDefinition() {

		RegionDefinition regionDefinition = RegionDefinition.from(this.mockRegion);

		this.template.createRegion(regionDefinition);

		verify(this.mockFunctionOperations, times(1))
			.executeAndExtract(eq(CreateRegionFunction.CREATE_REGION_FUNCTION_ID), eq(regionDefinition));
	}

	@Test
	public void createIndexCallsExecuteWithCreateIndexFunctionIdAndIndexDefinition() {

		IndexDefinition indexDefinition = IndexDefinition.from(this.mockIndex);

		this.template.createIndex(indexDefinition);

		verify(this.mockFunctionOperations, times(1))
			.executeAndExtract(eq(CreateIndexFunction.CREATE_INDEX_FUNCTION_ID), eq(indexDefinition));
	}

	@Test
	public void containsRegionInformationIsNullSafe() {
		assertThat(this.template.containsRegionInformation(null)).isFalse();
	}

	@Test
	public void containsRegionInformationReturnsFalseForNonObjectArrayResult() {
		assertThat(this.template.containsRegionInformation(newRegionInformation("TestRegion"))).isFalse();
	}

	@Test
	public void containsRegionInformationReturnsFalseForEmptyObjectArrayResult() {
		assertThat(this.template.containsRegionInformation(asArray())).isFalse();
	}

	@Test
	public void containsRegionInformationReturnsFalseForObjectArrayContainingNonRegionInformation() {
		assertThat(this.template.containsRegionInformation(asArray(mockRegion("TestRegion")))).isFalse();
	}

	@Test
	public void containsRegionInformationReturnsTrueForObjectArrayWithRegionInformation() {
		assertThat(this.template.containsRegionInformation(asArray(newRegionInformation("TestRegion"))))
			.isTrue();
	}
}

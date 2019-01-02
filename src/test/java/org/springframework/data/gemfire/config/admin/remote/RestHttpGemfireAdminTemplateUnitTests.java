/*
 * Copyright 2017-2019 the original author or authors.
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
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.net.URI;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.query.Index;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.data.gemfire.IndexType;
import org.springframework.data.gemfire.config.schema.definitions.IndexDefinition;
import org.springframework.data.gemfire.config.schema.definitions.RegionDefinition;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestOperations;

/**
 * Unit tests for {@link RestHttpGemfireAdminTemplate}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.admin.remote.RestHttpGemfireAdminTemplate
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class RestHttpGemfireAdminTemplateUnitTests {

	@Mock
	private ClientCache mockClientCache;

	@Mock
	private Index mockIndex;

	@Mock
	private Region mockRegion;

	private RestHttpGemfireAdminTemplate template;

	@Mock
	private RestOperations mockRestOperations;

	@Before
	public void setup() {

		this.template = new RestHttpGemfireAdminTemplate(this.mockClientCache) {
			@Override protected RestOperations newRestOperations() {
				return mockRestOperations;
			}
		};

		when(this.mockRegion.getName()).thenReturn("MockRegion");
		when(this.mockIndex.getName()).thenReturn("MockIndex");
		when(this.mockIndex.getIndexedExpression()).thenReturn("age");
		when(this.mockIndex.getFromClause()).thenReturn("/Customers");
		when(this.mockIndex.getType()).thenReturn(IndexType.FUNCTIONAL.getGemfireIndexType());
	}

	@Test
	public void constructDefaultRestHttpGemfireAdminTemplate() {

		RestHttpGemfireAdminTemplate template = new RestHttpGemfireAdminTemplate(this.mockClientCache);

		assertThat(template).isNotNull();
		assertThat(template.getClientCache()).isSameAs(this.mockClientCache);
		assertThat(template.getManagementRestApiUrl())
			.isEqualTo(String.format(RestHttpGemfireAdminTemplate.MANAGEMENT_REST_API_URL_TEMPLATE,
				RestHttpGemfireAdminTemplate.DEFAULT_HOST, RestHttpGemfireAdminTemplate.DEFAULT_PORT));
		assertThat(template.getRestOperations()).isNotNull();
	}

	@Test
	public void constructCustomRestHttpGemfireAdminTemplate() {

		RestHttpGemfireAdminTemplate template =
			new RestHttpGemfireAdminTemplate(this.mockClientCache, "skullbox", 8080);

		assertThat(template).isNotNull();
		assertThat(template.getClientCache()).isSameAs(this.mockClientCache);
		assertThat(template.getManagementRestApiUrl())
			.isEqualTo(String.format(RestHttpGemfireAdminTemplate.MANAGEMENT_REST_API_URL_TEMPLATE, "skullbox", 8080));
		assertThat(template.getRestOperations()).isNotNull();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void createIndexCallsGemFireManagementRestApi() {

		IndexDefinition indexDefinition = IndexDefinition.from(this.mockIndex);

		when(this.mockRestOperations.exchange(any(RequestEntity.class), eq(String.class))).thenAnswer(invocation -> {

			RequestEntity requestEntity = invocation.getArgument(0);

			assertThat(requestEntity).isNotNull();
			assertThat(requestEntity.getMethod()).isEqualTo(HttpMethod.POST);
			assertThat(requestEntity.getUrl()).isEqualTo(URI.create("http://localhost:7070/gemfire/v1/indexes"));

			HttpHeaders headers = requestEntity.getHeaders();

			assertThat(headers).isNotNull();
			assertThat(headers.getContentType()).isEqualTo(MediaType.APPLICATION_FORM_URLENCODED);

			Object body = requestEntity.getBody();

			assertThat(body).isInstanceOf(MultiValueMap.class);

			MultiValueMap<String, Object> requestBody = (MultiValueMap<String, Object>) body;

			assertThat(requestBody.getFirst("name")).isEqualTo(indexDefinition.getName());
			assertThat(requestBody.getFirst("expression")).isEqualTo(indexDefinition.getExpression());
			assertThat(requestBody.getFirst("region")).isEqualTo(indexDefinition.getFromClause());
			assertThat(requestBody.getFirst("type")).isEqualTo(indexDefinition.getIndexType().toString());

			return new ResponseEntity(HttpStatus.OK);
		});

		this.template.createIndex(indexDefinition);

		verify(this.mockRestOperations, times(1))
			.exchange(isA(RequestEntity.class), eq(String.class));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void createRegionCallsGemFireManagementRestApi() {

		RegionDefinition regionDefinition = RegionDefinition.from(this.mockRegion);

		when(this.mockRestOperations.exchange(any(RequestEntity.class), eq(String.class))).thenAnswer(invocation -> {

			RequestEntity requestEntity = invocation.getArgument(0);

			assertThat(requestEntity).isNotNull();
			assertThat(requestEntity.getMethod()).isEqualTo(HttpMethod.POST);
			assertThat(requestEntity.getUrl()).isEqualTo(URI.create("http://localhost:7070/gemfire/v1/regions"));

			HttpHeaders headers = requestEntity.getHeaders();

			assertThat(headers).isNotNull();
			assertThat(headers.getContentType()).isEqualTo(MediaType.APPLICATION_FORM_URLENCODED);

			Object body = requestEntity.getBody();

			assertThat(body).isInstanceOf(MultiValueMap.class);

			MultiValueMap<String, Object> requestBody = (MultiValueMap<String, Object>) body;

			assertThat(requestBody.getFirst("name")).isEqualTo(regionDefinition.getName());
			assertThat(requestBody.getFirst("type")).isEqualTo(regionDefinition.getRegionShortcut().toString());
			assertThat(requestBody.getFirst("skip-if-exists"))
				.isEqualTo(String.valueOf(RestHttpGemfireAdminTemplate.CREATE_REGION_SKIP_IF_EXISTS_DEFAULT));

			return new ResponseEntity(HttpStatus.OK);
		});

		this.template.createRegion(regionDefinition);

		verify(this.mockRestOperations, times(1))
			.exchange(isA(RequestEntity.class), eq(String.class));
	}
}

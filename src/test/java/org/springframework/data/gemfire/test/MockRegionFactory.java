/*
 * Copyright 2002-2013 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.test;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;

import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import com.gemstone.gemfire.cache.AttributesFactory;
import com.gemstone.gemfire.cache.CacheListener;
import com.gemstone.gemfire.cache.CacheLoader;
import com.gemstone.gemfire.cache.CacheWriter;
import com.gemstone.gemfire.cache.CustomExpiry;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.DiskWriteAttributes;
import com.gemstone.gemfire.cache.EvictionAttributes;
import com.gemstone.gemfire.cache.ExpirationAttributes;
import com.gemstone.gemfire.cache.MembershipAttributes;
import com.gemstone.gemfire.cache.PartitionAttributes;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.RegionFactory;
import com.gemstone.gemfire.cache.RegionService;
import com.gemstone.gemfire.cache.Scope;
import com.gemstone.gemfire.cache.SubscriptionAttributes;
import com.gemstone.gemfire.cache.query.QueryService;

/**
 * @author David Turanski
 * @author John Blum
 */
@SuppressWarnings("deprecation")
public class MockRegionFactory<K,V>   {

	private static QueryService queryService =  mock(QueryService.class);
	private static RegionService regionService = mock(RegionService.class);

	private AttributesFactory<K,V> attributesFactory;

	private final StubCache cache;

	public MockRegionFactory(StubCache cache) {
		this.cache = cache;
	}

	public RegionFactory<K, V> createMockRegionFactory() {
		return createMockRegionFactory(null);
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public RegionFactory<K, V> createMockRegionFactory(RegionAttributes<K,V> attributes) {
		attributesFactory = (attributes != null ? new AttributesFactory<K,V>(attributes)
			: new AttributesFactory<K,V>());

		//Workaround for GemFire bug
		if (attributes !=null) {
			attributesFactory.setLockGrantor(attributes.isLockGrantor());
		}

		final RegionFactory<K, V> regionFactory = mock(RegionFactory.class);

		when(regionFactory.create(anyString())).thenAnswer(new Answer<Region>() {
			@Override
			public Region answer(InvocationOnMock invocation) throws Throwable {
				String name = (String) invocation.getArguments()[0];
				Region region = mockRegion(name);
				cache.allRegions().put(name, region);
				return region;
			}
		});

		when(regionFactory.createSubregion(any(Region.class),anyString())).thenAnswer(new Answer<Region>() {
			@Override
			public Region answer(InvocationOnMock invocation) throws Throwable {
				Region parent = (Region) invocation.getArguments()[0];
				String name = (String) invocation.getArguments()[1];
				String parentRegionName = null;

				for (String key: cache.allRegions().keySet()) {
					if (cache.allRegions().get(key).equals(parent)) {
						parentRegionName = key;
					}
				}

				assert parentRegionName != null : "The parent Region name was null!";

				String subRegionName = (parentRegionName.startsWith("/") ? parentRegionName+"/"+name
					: "/"+parentRegionName+"/"+ name);

				Region subRegion = mockRegion(subRegionName);

				cache.allRegions().put(subRegionName, subRegion);

				return subRegion;
			}
		});

		when(regionFactory.setCacheLoader(any(CacheLoader.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				CacheLoader val =  (CacheLoader)invocation.getArguments()[0];
				attributesFactory.setCacheLoader(val);
				return regionFactory;
			}
		});

		when(regionFactory.setCacheWriter(any(CacheWriter.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				CacheWriter val =  (CacheWriter)invocation.getArguments()[0];
				attributesFactory.setCacheWriter(val);
				return regionFactory;
			}
		});


		when(regionFactory.addAsyncEventQueueId(anyString())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				String val =  (String)invocation.getArguments()[0];
				attributesFactory.addAsyncEventQueueId(val);
				return regionFactory;
			}
		});

		when(regionFactory.addCacheListener(any(CacheListener.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				CacheListener val =  (CacheListener)invocation.getArguments()[0];
				attributesFactory.addCacheListener(val);
				return regionFactory;
			}
		});

		when(regionFactory.setEvictionAttributes(any(EvictionAttributes.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				EvictionAttributes val =  (EvictionAttributes)invocation.getArguments()[0];
				attributesFactory.setEvictionAttributes(val);
				return regionFactory;
			}
		});

		when(regionFactory.setEntryIdleTimeout(any(ExpirationAttributes.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				ExpirationAttributes val =  (ExpirationAttributes)invocation.getArguments()[0];
				attributesFactory.setEntryIdleTimeout(val);
				return regionFactory;
			}
		});

		when(regionFactory.setCustomEntryIdleTimeout(any(CustomExpiry.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				CustomExpiry val =  (CustomExpiry)invocation.getArguments()[0];
				attributesFactory.setCustomEntryIdleTimeout(val);
				return regionFactory;
			}
		});

		when(regionFactory.setEntryTimeToLive(any(ExpirationAttributes.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				ExpirationAttributes val =  (ExpirationAttributes)invocation.getArguments()[0];
				attributesFactory.setEntryTimeToLive(val);
				return regionFactory;
			}
		});

		when(regionFactory.setCustomEntryTimeToLive(any(CustomExpiry.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				CustomExpiry val =  (CustomExpiry)invocation.getArguments()[0];
				attributesFactory.setCustomEntryTimeToLive(val);
				return regionFactory;
			}
		});

		when(regionFactory.setRegionIdleTimeout(any(ExpirationAttributes.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				ExpirationAttributes val =  (ExpirationAttributes)invocation.getArguments()[0];
				attributesFactory.setRegionIdleTimeout(val);
				return regionFactory;
			}
		});

		when(regionFactory.setRegionTimeToLive(any(ExpirationAttributes.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				ExpirationAttributes val =  (ExpirationAttributes)invocation.getArguments()[0];
				attributesFactory.setRegionTimeToLive(val);
				return regionFactory;
			}
		});

		when(regionFactory.setScope(any(Scope.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				Scope val =  (Scope)invocation.getArguments()[0];
				attributesFactory.setScope(val);
				return regionFactory;
			}
		});

		when(regionFactory.setDataPolicy(any(DataPolicy.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				DataPolicy val =  (DataPolicy)invocation.getArguments()[0];
				attributesFactory.setDataPolicy(val);
				return regionFactory;
			}
		});

		when(regionFactory.setEarlyAck(anyBoolean())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				boolean val =  (Boolean)invocation.getArguments()[0];
				attributesFactory.setEarlyAck(val);
				return regionFactory;
			}
		});

		when(regionFactory.setMulticastEnabled(anyBoolean())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				boolean val =  (Boolean)invocation.getArguments()[0];
				attributesFactory.setMulticastEnabled(val);
				return regionFactory;
			}
		});

		when(regionFactory.setPoolName(anyString())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				String val =  (String)invocation.getArguments()[0];
				attributesFactory.setPoolName(val);
				return regionFactory;
			}
		});

		when(regionFactory.setEnableGateway(anyBoolean())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				boolean val =  (Boolean)invocation.getArguments()[0];
				attributesFactory.setEnableGateway(val);
				return regionFactory;
			}
		});

		when(regionFactory.setEnableAsyncConflation(anyBoolean())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				boolean val =  (Boolean)invocation.getArguments()[0];
				attributesFactory.setEnableAsyncConflation(val);
				return regionFactory;
			}
		});

		when(regionFactory.setEnableSubscriptionConflation(anyBoolean())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				boolean val =  (Boolean)invocation.getArguments()[0];
				attributesFactory.setEnableSubscriptionConflation(val);
				return regionFactory;
			}
		});

		when(regionFactory.setKeyConstraint(any(Class.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				Class val =  (Class)invocation.getArguments()[0];
				attributesFactory.setKeyConstraint(val);
				return regionFactory;
			}
		});

		when(regionFactory.setValueConstraint(any(Class.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				Class val =  (Class)invocation.getArguments()[0];
				attributesFactory.setValueConstraint(val);
				return regionFactory;
			}
		});

		when(regionFactory.setInitialCapacity(anyInt())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				int val =  (Integer)invocation.getArguments()[0];
				System.out.println("setInitialCapacity " + val);
				attributesFactory.setInitialCapacity(val);
				return regionFactory;
			}
		});

		when(regionFactory.setLoadFactor(anyInt())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				int val =  (Integer)invocation.getArguments()[0];
				attributesFactory.setLoadFactor(val);
				return regionFactory;
			}
		});

		when(regionFactory.setConcurrencyLevel(anyInt())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				int val =  (Integer)invocation.getArguments()[0];
				attributesFactory.setConcurrencyLevel(val);
				return regionFactory;
			}
		});

		when(regionFactory.setConcurrencyChecksEnabled(anyBoolean())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				boolean val =  (Boolean)invocation.getArguments()[0];
				attributesFactory.setConcurrencyChecksEnabled(val);
				return regionFactory;
			}
		});

		when(regionFactory.setDiskWriteAttributes(any(DiskWriteAttributes.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				DiskWriteAttributes val =  (DiskWriteAttributes)invocation.getArguments()[0];
				attributesFactory.setDiskWriteAttributes(val);
				return regionFactory;
			}
		});

		when(regionFactory.setDiskStoreName(anyString())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				String val =  (String)invocation.getArguments()[0];
				attributesFactory.setDiskStoreName(val);
				return regionFactory;
			}
		});

		when(regionFactory.setDiskSynchronous(anyBoolean())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				boolean val =  (Boolean)invocation.getArguments()[0];
				attributesFactory.setDiskSynchronous(val);
				return regionFactory;
			}
		});

		when(regionFactory.setDiskDirs(any(File[].class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				File[] val =  (File[])invocation.getArguments()[0];
				attributesFactory.setDiskDirs(val);
				return regionFactory;
			}
		});

		when(regionFactory.setDiskDirsAndSizes(any(File[].class),any(int[].class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				File[] val0 =  (File[])invocation.getArguments()[0];
				int[] val1 = (int[])invocation.getArguments()[1];
				attributesFactory.setDiskDirsAndSizes(val0,val1);
				return regionFactory;
			}
		});

		when(regionFactory.setPartitionAttributes(any(PartitionAttributes.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				PartitionAttributes val =  (PartitionAttributes)invocation.getArguments()[0];
				attributesFactory.setPartitionAttributes(val);
				return regionFactory;
			}
		});

		when(regionFactory.setMembershipAttributes(any(MembershipAttributes.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				MembershipAttributes val =  (MembershipAttributes)invocation.getArguments()[0];
				attributesFactory.setMembershipAttributes(val);
				return regionFactory;
			}
		});

		when(regionFactory.setIndexMaintenanceSynchronous(anyBoolean())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				boolean val =  (Boolean)invocation.getArguments()[0];
				attributesFactory.setIndexMaintenanceSynchronous(val);
				return regionFactory;
			}
		});

		when(regionFactory.setStatisticsEnabled(anyBoolean())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				boolean val =  (Boolean)invocation.getArguments()[0];
				attributesFactory.setStatisticsEnabled(val);
				return regionFactory;
			}
		});

		when(regionFactory.setIgnoreJTA(anyBoolean())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				boolean val =  (Boolean)invocation.getArguments()[0];
				attributesFactory.setIgnoreJTA(val);
				return regionFactory;
			}
		});

		when(regionFactory.setLockGrantor(anyBoolean())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				boolean val =  (Boolean)invocation.getArguments()[0];
				System.out.println("setting lock grantor to " + val);
				attributesFactory.setLockGrantor(val);
				return regionFactory;
			}
		});

		when(regionFactory.setSubscriptionAttributes(any(SubscriptionAttributes.class))).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				SubscriptionAttributes val =  (SubscriptionAttributes)invocation.getArguments()[0];
				attributesFactory.setSubscriptionAttributes(val);
				return regionFactory;
			}
		});

		when(regionFactory.setGatewayHubId(anyString())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				String val =  (String)invocation.getArguments()[0];
				attributesFactory.setGatewayHubId(val);
				return regionFactory;
			}
		});

		when(regionFactory.setCloningEnabled(anyBoolean())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				boolean val =  (Boolean)invocation.getArguments()[0];
				attributesFactory.setCloningEnabled(val);
				return regionFactory;
			}
		});

		when(regionFactory.addGatewaySenderId(anyString())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				String val =  (String)invocation.getArguments()[0];
				attributesFactory.addGatewaySenderId(val);
				return regionFactory;
			}
		});

		when(regionFactory.addAsyncEventQueueId(anyString())).thenAnswer(new Answer<RegionFactory>(){
			@Override
			public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
				String val =  (String)invocation.getArguments()[0];
				attributesFactory.addAsyncEventQueueId(val);
				return regionFactory;
			}
		});


		return regionFactory;
	}

	@SuppressWarnings("rawtypes")
	RegionFactory createRegionFactory() {
		return createMockRegionFactory();
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public Region mockRegion(String name) {
		RegionService regionService = mockRegionService();
		Region region = mock(Region.class);

		when(region.getRegionService()).thenReturn(regionService);

		when(region.getAttributes()).thenAnswer(new Answer<RegionAttributes>() {
			@Override
			public RegionAttributes answer(InvocationOnMock invocation) throws Throwable {
				RegionAttributes attributes = attributesFactory.create();
				return attributes;
			}
		});

		when(region.getFullPath()).thenReturn(name);
		when(region.getName()).thenReturn(name);

		when(region.getSubregion(anyString())).thenAnswer(new Answer<Region>() {
			@Override
			public Region answer(InvocationOnMock invocation) throws Throwable {
				Region parent = (Region) invocation.getMock();

				String parentRegionName = parent.getName();
				String subRegionName = (String) invocation.getArguments()[0];
				String subRegionPath = (parentRegionName.startsWith("/") ? parentRegionName+"/"+subRegionName
					: "/"+parentRegionName+"/"+subRegionName);

				Region region = cache.getRegion(subRegionPath);

				return region;
			}
		});

		when(region.createSubregion(anyString(), any(RegionAttributes.class))).thenAnswer(new Answer<Region>() {
			@Override
			public Region answer(InvocationOnMock invocation) throws Throwable {
				String name = (String) invocation.getArguments()[0];
				RegionAttributes attributes = (RegionAttributes) invocation.getArguments()[1];

				Region parent = (Region) invocation.getMock();
				String parentName = parent.getName();
				String regionName = parentName.startsWith("/") ? parentName+"/"+name : "/"+parentName+"/"+ name;

				Region subRegion = new MockRegionFactory(cache).createMockRegionFactory(attributes).create(regionName);
				when(subRegion.getFullPath()).thenReturn(regionName);

				cache.allRegions().put(regionName, subRegion);

				return subRegion;
			}
		});

		return region;
	}

	public static RegionService mockRegionService() {
		when(regionService.getQueryService()).thenReturn(queryService);
		return regionService;
	}

	public static QueryService mockQueryService() {
		return queryService;
	}


}

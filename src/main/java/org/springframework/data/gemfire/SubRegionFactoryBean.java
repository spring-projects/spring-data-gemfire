package org.springframework.data.gemfire;

import java.util.List;

import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;

import com.gemstone.gemfire.cache.AttributesFactory;

public class SubRegionFactoryBean<K,V> extends AttributesFactory<K,V> implements FactoryBean<SubRegion<K, V>>, InitializingBean {
	private String name;
	private SubRegion<K,V> subRegion;
	private String regionName;
	private List<SubRegion<?,?>> subRegions;

	
	public void setName(String name) {
		this.name = name;
	}

	public void setSubRegions(List<SubRegion<?, ?>> subRegions) {
		this.subRegions = subRegions;
	}

	@Override
	public void afterPropertiesSet() throws Exception {	
		this.subRegion = new SubRegion<K,V>(name,create());
		this.subRegion.setSubRegions(this.subRegions);
	}

	@Override
	public SubRegion<K, V> getObject() throws Exception {
		return this.subRegion;
	}

	@Override
	public Class<?> getObjectType() {
		return SubRegion.class;
	}

	@Override
	public boolean isSingleton() {
		return true;
	}

}

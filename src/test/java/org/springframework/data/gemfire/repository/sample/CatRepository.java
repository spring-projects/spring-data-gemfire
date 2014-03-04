package org.springframework.data.gemfire.repository.sample;

import org.springframework.data.gemfire.mapping.Region;
import org.springframework.data.gemfire.repository.GemfireRepository;
import org.springframework.data.gemfire.repository.Query;

@Region("Cats")
public interface CatRepository extends GemfireRepository<Animal, Long> {

	Animal findByName(String name);

	@Query("SELECT * FROM /Foo x WHERE x.name = $1")
	Animal findBy(String name);

}
